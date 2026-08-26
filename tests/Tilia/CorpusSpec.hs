{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Formatting other people's Haskell.
module Tilia.CorpusSpec (spec) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (join)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Test.Hspec hiding (Example, after, before, example)
import Text.Read (readMaybe)
import Tilia.Corpus
import Tilia.Diff (Colours, coloursFor, diff)
import Tilia.Equivalence (commentDifference, syntaxDifference)
import Tilia.Parser
  ( ParsedModule (..),
    defaultParserConfig,
    describeParseError,
    parseText,
    movesPositions,
  )
import Tilia.Doc (defaultRenderOptions, printDoc)
import Tilia.Render (renderModule)
import Tilia.TestConfig (exampleSettings)

spec :: Spec
spec = do
  corpusSpec vendoredExamples
  corpusSpec ormoluExamples
  corpusSpec ghcTestSuite

corpusSpec :: Corpus -> Spec
corpusSpec corpus =
  describe (corpusName corpus) $
    runIO (obtain corpus) >>= \case
      Left problem ->
        it "is available" . pendingWith $
          "corpus not on this machine and could not be fetched: " <> T.unpack problem
      Right examples -> do
        limit <- runIO exampleTimeout
        colours <- runIO coloursFor

        let declines = Set.fromList (corpusDeclined corpus)
        for_ examples $ \example ->
          it (exampleName example) $ do
            result <- check colours limit example
            for_ (complaint (Set.member (exampleName example) declines) result) $
              expectationFailure . T.unpack

-- | How long one example gets before it is called a failure.
exampleTimeout :: IO Int
exampleTimeout =
  lookupEnv "TILIA_CORPUS_TIMEOUT" >>= \case
    Just s | Just seconds <- readMaybe s -> pure (seconds * 1_000_000)
    _ -> pure (60 * 1_000_000)

----------------------------------------------------------------------------
-- Checking one example

-- | What running the formatter over one example established.
--
-- Every one of these but 'Formatted' and an expected 'Declined' is a
-- failure. An example the formatter cannot read is not an example we are
-- excused from: it is one the corpus was supposed to have been told about,
-- in 'corpusSkip', where it stops being an example at all. Anything that
-- stops parsing without being named there is a change in what we can read,
-- and that is exactly the thing worth hearing about.
data Result
  = -- | The bytes are not UTF-8, so there is nothing to parse. A Haskell
    -- source file is UTF-8 by definition, and GHC's test suite carries a
    -- few that are not on purpose.
    NotUtf8
  | -- | GHC's own parser could not read it. Its test suite is full of files
    -- meant not to compile, and of CPP that we do not expand.
    DoesNotParse Text
  | -- | The input is Haskell, and the formatter refuses to rewrite it.
    Declined
  | -- | A property does not hold.
    Broken Text
  | -- | Nothing was found wrong with it.
    Formatted

-- | What is wrong with what one example produced, if anything.
complaint ::
  -- | Does the corpus say this one should be declined?
  Bool ->
  Result ->
  Maybe Text
complaint declines = \case
  Broken why -> Just why
  NotUtf8 -> Just (unlisted "is not UTF-8")
  DoesNotParse where' -> Just (unlisted ("does not parse, at " <> where'))
  Declined
    | declines -> Nothing
    | otherwise -> Just "the formatter declined this, and the corpus does not say it should"
  Formatted
    | declines -> Just "the corpus says this should be declined, and it was not"
    | otherwise -> Nothing
  where
    unlisted why =
      "this " <> why <> ", and the corpus does not list it under corpusSkip"

check :: Colours -> Int -> Example -> IO Result
check colours limit example = do
  source <- readUtf8 (exampleInput example)
  expected <- traverse readUtf8 (exampleReference example)
  case source of
    Nothing -> pure NotUtf8
    Just text -> guarded limit (checkPure colours text (join expected))

-- | Read a file that is supposed to be a Haskell module.
--
-- Decoded here rather than by 'T.readFile', which would use whatever
-- encoding the machine happens to be set to and fail on a byte it did not
-- expect. A Haskell source file is UTF-8 by definition, so one that is not
-- is not a module, and 'Nothing' says so.
readUtf8 :: FilePath -> IO (Maybe Text)
readUtf8 path = either (const Nothing) Just . decodeUtf8' <$> BS.readFile path

-- | Run a check, turning a crash or a hang into a failure rather than into a
-- dead test run.
--
-- The result is forced here rather than left to the assertion, so that an
-- error raised deep inside the printer is reported against the example that
-- provoked it, and so that the time it takes is time the limit covers.
guarded :: Int -> Result -> IO Result
guarded limit result =
  try (timeout limit (evaluate (forced result))) >>= \case
    Left (e :: SomeException) ->
      pure (Broken ("the formatter raised an error: " <> firstLine (T.pack (show e))))
    Right Nothing ->
      pure (Broken ("the formatter did not finish within " <> T.pack (show (limit `div` 1_000_000)) <> "s"))
    Right (Just settled) -> pure settled
  where
    forced = \case
      NotUtf8 -> NotUtf8
      DoesNotParse where' -> T.length where' `seq` DoesNotParse where'
      Declined -> Declined
      Broken why -> T.length why `seq` Broken why
      Formatted -> Formatted
    firstLine = T.strip . T.takeWhile (/= '\n')

-- | Everything that can be established about one example without doing any
-- more input or output.
--
-- Each failure carries a diff, and which two things are being compared is
-- chosen to be the pair the reader would have compared themselves. A
-- property that broke somewhere inside the module is shown against the
-- input, because the question is what formatting did to it. Formatting that
-- will not settle is shown against the first pass, because the first pass
-- is the fixed point it was supposed to have reached and everything they
-- agree on is beside the point.
checkPure :: Colours -> Text -> Maybe Text -> Result
checkPure colours source expected
  | movesPositions source = Declined
  | otherwise = case parseText defaultParserConfig "<corpus>" source of
      Left problem -> DoesNotParse (describeParseError problem)
      Right before ->
        let formatted = render before
            against name = diff colours ("input", name) source formatted
         in case parse formatted of
              Nothing ->
                Broken
                  ( "the formatted output does not parse\n"
                      <> against "output (does not parse)"
                  )
              Just after
                | Just difference <- syntaxDifference (pmModule before) (pmModule after) ->
                    Broken
                      ( "a different program: "
                          <> difference
                          <> "\n"
                          <> against "output"
                      )
                | Just difference <-
                    commentDifference
                      (pmModule before, pmModule after)
                      (pmComments before)
                      (pmComments after) ->
                    Broken
                      ( "comments: "
                          <> difference
                          <> "\n"
                          <> against "output"
                      )
                | settled <- render after,
                  settled /= formatted ->
                    Broken
                      ( "formatting is non-idempotent\n"
                          <> diff colours ("first pass", "second pass") formatted settled
                      )
                -- A corpus that says what the answer should be is believed.
                -- There is no tolerance to spend: an example we lay out
                -- differently is one we have not finished with, whatever the
                -- reason, and a proportion of them being right says nothing
                -- about any particular one.
                | Just reference <- expected,
                  reference /= formatted ->
                    Broken
                      ( "does not match the corpus's expected output\n"
                          <> diff colours ("expected", "ours") reference formatted
                      )
                | otherwise -> Formatted
  where
    parse = either (const Nothing) Just . parseText defaultParserConfig "<corpus>"

    -- The settings are worked out from the module in hand rather than fixed
    -- once, because they depend on what it imports; see "Tilia.TestConfig".
    render parsed =
      printDoc
        defaultRenderOptions
        (renderModule (exampleSettings source (pmModule parsed)) parsed)
