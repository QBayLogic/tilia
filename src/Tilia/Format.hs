{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Formatting a file, with everything the project can tell us about it.
module Tilia.Format
  ( FormatError (..),
    describeFormatError,
    formatErrorExitCode,
    refused,
    Session,
    newSession,
    formatSource,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Choice (Choice, isTrue)
import Data.Foldable (traverse_)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Tilia.Cpp
  ( CppError (..),
    blankCpp,
    branchLeaves,
    describeCppError,
    formatWithCpp,
    usesCpp,
  )
import Tilia.Equivalence (commentDifference, syntaxDifference)
import Tilia.Fixity (Fixity, OpName (..), Unknown (..), unknownOperators)
import Tilia.Fixity.Plan (loadPlan, newResolver, scopeFor)
import Tilia.Parser
  ( ParseError,
    ParserConfig,
    describeParseError,
    parseModule,
    parserConfigFor,
    pmModule,
    pmSource,
  )
import Tilia.Pragma (effectiveExtensions, movesPositions)
import Tilia.Doc (defaultRenderOptions, printDoc)
import Tilia.Package
  ( PackageProblem (..),
    PackageReader,
    describePackageProblem,
    newPackageReader,
  )
import Tilia.Palette (Color (Operator, Place), Palette, paint)
import Tilia.Project (ProjectRoot (..), findProjectRoot)
import Tilia.Render (RenderConfig (..), defaultRenderConfig, renderModule)
import Tilia.Source (comments)

-- | Why a file could not be formatted.
data FormatError
  = -- | No @cabal.project@, @stack.yaml@ or @.cabal@ file above it.
    NoProject FilePath
  | -- | A project, but no build plan we could read or produce. The text is
    -- whatever @cabal@ had to say about it.
    NoBuildPlan FilePath Text
  | -- | We failed to read .cabal file.
    NoPackage FilePath PackageProblem
  | -- | The file is not Haskell we can parse.
    NotParsed ParseError
  | -- | The file carries @{-# LINE #-}@ or @{-# COLUMN #-}@ pragmas.
    PositionPragmas FilePath
  | -- | The file uses the preprocessor in a way we cannot handle.
    CppUnsupported FilePath CppError
  | -- | An operator the file uses has a fixity we could not establish.
    UnknownFixity FilePath [(OpName, Unknown)]
  | -- | The file could not be read at all.
    Unreadable FilePath Text
  | -- | Formatting the file changed its AST.
    NotEquivalent FilePath Text
  | -- | Formatting is not idempotent.
    NotIdempotent FilePath Text

-- | Say what went wrong, in one line.
describeFormatError :: Palette -> FormatError -> Text
describeFormatError palette = \case
  NoProject path ->
    "no project above " <> file path <> ": expected a cabal.project, a stack.yaml or a .cabal file"
  NoBuildPlan root reason ->
    "no build plan for " <> file root <> ": " <> reason
  NoPackage path problem ->
    "cannot tell what "
      <> file path
      <> " is written in: "
      <> describePackageProblem problem
  NotParsed e -> "cannot parse " <> located (describeParseError e)
  PositionPragmas path ->
    "will not format " <> file path <> ": it uses {-# LINE #-} pragmas, and no reformatting can leave those true"
  CppUnsupported path why ->
    "will not format " <> file path <> ": " <> describeCppError why
  Unreadable path why -> "cannot read " <> file path <> ": " <> why
  NotEquivalent path why ->
    "formatting " <> file path <> " changed the program: " <> why
  NotIdempotent path why ->
    "formatting " <> file path <> " is not idempotent: " <> why
  UnknownFixity path unknown ->
    "will not format "
      <> file path
      <> ": the fixity of "
      <> T.intercalate ", " (map saying unknown)
    where
      saying (OpName op, why) = paint palette Operator op <> " " <> because why
      because = \case
        NotRead missing ->
          "may be declared in "
            <> T.intercalate " or " (map (paint palette Place) (NE.toList missing))
            <> ", which could not be read"
        Ambiguous -> "is declared differently by two modules in scope"
  where
    file = paint palette Place . T.pack
    -- A parse error opens with the span the parser rendered, which opens
    -- with the file. Set that much of it like every other file name here.
    located t = case T.breakOn ":" t of
      (where', rest) -> paint palette Place where' <> rest

-- | The exit status a failure should leave behind.
formatErrorExitCode :: FormatError -> Int
formatErrorExitCode = \case
  NoProject {} -> 2
  NoBuildPlan {} -> 3
  NotParsed {} -> 4
  PositionPragmas {} -> 5
  NoPackage _ problem -> case problem of
    NoPackageFile -> 6
    PackageUnreadable {} -> 6
    PackageMalformed {} -> 7
    FileUnclaimed {} -> 8
  UnknownFixity {} -> 15
  Unreadable {} -> 16
  NotEquivalent {} -> 17
  NotIdempotent {} -> 18
  CppUnsupported _ why -> case why of
    UnhandledDirective {} -> 9
    UnsplittableConditional -> 10
    TooManyConfigurations -> 11
    ConfigurationNotParsed {} -> 12
    DirectiveUnplaceable {} -> 13
    DirectiveInQuotedText {} -> 14

-- | Did we decline to format the file, rather than fail to?
refused :: FormatError -> Bool
refused = \case
  PositionPragmas {} -> True
  CppUnsupported {} -> True
  UnknownFixity {} -> True
  NotParsed {} -> False
  NoPackage {} -> False
  Unreadable {} -> False
  NotEquivalent {} -> False
  NotIdempotent {} -> False
  NoProject {} -> False
  NoBuildPlan {} -> False

-- | What a run works out once and then uses for every file.
--
-- Finding the project, solving its build plan and building a resolver cost
-- about as much as formatting a small file, and none of it depends on which
-- file is being formatted.
data Session = Session
  { -- | What each module in scope exports.
    sessionResolve :: Text -> IO (Maybe (Map OpName Fixity)),
    -- | What each file's package puts in force.
    sessionPackage :: PackageReader,
    -- | Whether to check AST equivalence.
    sessionChecksAst :: Choice "checkAst",
    -- | Whether to check idempotence.
    sessionChecksIdempotence :: Choice "checkIdempotence"
  }

-- | Settle everything that does not depend on the file being formatted.
newSession ::
  -- | Where to start looking for the project
  FilePath ->
  -- | Check AST equivalence.
  Choice "checkAst" ->
  -- | Check idempotence.
  Choice "checkIdempotence" ->
  IO (Either FormatError Session)
newSession start checksAst checksIdempotence = runExceptT $ do
  root <- prPath <$> (need (NoProject start) =<< liftIO (findProjectRoot start))
  plan <- orElse (NoBuildPlan root) =<< liftIO (loadPlan root)
  resolve <- liftIO (newResolver plan)
  askPackage <- liftIO newPackageReader
  pure
    Session
      { sessionResolve = resolve,
        sessionPackage = askPackage,
        sessionChecksAst = checksAst,
        sessionChecksIdempotence = checksIdempotence
      }
  where
    need :: FormatError -> Maybe a -> ExceptT FormatError IO a
    need e = maybe (throwE e) pure

-- | Format source that has already been read.
--
-- The text is passed in rather than read here because a caller that means
-- to compare the two needs the original anyway, and reading a file twice to
-- format it once is the sort of thing this is trying to stop doing.
formatSource ::
  -- | What the run has worked out already
  Session ->
  -- | The file the source came from, for reporting and for its package
  FilePath ->
  -- | The source
  Text ->
  -- | Result
  IO (Either FormatError Text)
formatSource session path source = runExceptT $ do
  when (movesPositions source) $
    throwE (PositionPragmas path)
  package <- orElse (NoPackage path) =<< liftIO (sessionPackage session path)
  let resolve = sessionResolve session
      config = parserConfigFor package
      cpp = usesCpp (effectiveExtensions package source) source
      renderConfigFor extensions hsModule = do
        scope <- liftIO (scopeFor resolve hsModule)
        case unknownOperators scope hsModule of
          [] ->
            pure
              defaultRenderConfig
                { rcExtensions = extensions,
                  rcScope = Just scope
                }
          unknown -> throwE (UnknownFixity path unknown)
      formatting text = do
        let inForce = effectiveExtensions package text
            extensions = Set.fromList inForce
        if usesCpp inForce text
          then do
            render <- case parseModule config path (blankCpp text) of
              Left _ -> pure defaultRenderConfig {rcExtensions = extensions}
              Right whole -> renderConfigFor extensions (pmModule whole)
            orElse
              (CppUnsupported path)
              (formatWithCpp config render path text)
          else do
            parsed <- orElse NotParsed (parseModule config path text)
            render <- renderConfigFor extensions (pmModule parsed)
            pure (printDoc defaultRenderOptions (renderModule render parsed))
  formatted <- formatting source
  when (isTrue (sessionChecksAst session)) $
    traverse_
      (throwE . NotEquivalent path)
      (rewritten config cpp path source formatted)
  when (isTrue (sessionChecksIdempotence session)) $ do
    settled <- formatting formatted
    when (settled /= formatted) $
      throwE (NotIdempotent path (whereTheyDiffer formatted settled))
  pure formatted

-- | Where two spellings of the same file first disagree.
whereTheyDiffer :: Text -> Text -> Text
whereTheyDiffer before after =
  case [n | (n, one, two) <- zip3 [1 :: Int ..] first second, one /= two] of
    (n : _) -> "line " <> tshow n <> " differs"
    [] ->
      "the second pass came out "
        <> tshow (length second)
        <> " lines long where the first came out "
        <> tshow (length first)
  where
    first = T.lines before
    second = T.lines after
    tshow :: Int -> Text
    tshow = T.pack . show

-- | What formatting changed about the program.
--
-- A file with conditionals is compared one configuration at a time, because
-- the text as it stands is not a program: the branches only make one once
-- the preprocessor has chosen between them.
rewritten ::
  -- | How to parse both sides
  ParserConfig ->
  -- | Whether the file uses the preprocessor
  Bool ->
  -- | The file, for the parser's messages
  FilePath ->
  -- | What was read
  Text ->
  -- | What was printed
  Text ->
  Maybe Text
rewritten config cpp path before after
  | not cpp = difference before after
  | otherwise = case (branchLeaves before, branchLeaves after) of
      -- Neither can really happen: a source that would not split never got
      -- as far as being formatted. Saying so beats saying nothing.
      (Left _, _) -> Just "the input could not be split into configurations"
      (_, Left _) -> Just "the output could not be split into configurations"
      (Right went, Right came)
        | length went /= length came ->
            Just
              ( "the output has "
                  <> tshow (length came)
                  <> " configurations where the input had "
                  <> tshow (length went)
              )
        | otherwise ->
            firstJust
              [ ("in one configuration, " <>) <$> difference b a
              | (b, a) <- zip went came
              ]
  where
    difference b a = case (parseModule config path b, parseModule config path a) of
      -- The input parsed once already, or there would be nothing to compare.
      (Left _, _) -> Nothing
      (_, Left e) ->
        Just ("the formatted output does not parse: " <> describeParseError e)
      (Right b', Right a') ->
        syntaxDifference (pmModule b') (pmModule a')
          <|> commentDifference
            (pmModule b', pmModule a')
            (comments (pmSource b'))
            (comments (pmSource a'))
    firstJust = foldr (<|>) Nothing
    tshow :: Int -> Text
    tshow = T.pack . show

-- | Give up with the given error where there is one to give up over.
orElse :: (e -> FormatError) -> Either e a -> ExceptT FormatError IO a
orElse f = either (throwE . f) pure
