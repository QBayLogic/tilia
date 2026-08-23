{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Turning source text into a syntax tree and a comment stream.
module Tilia.Parser
  ( -- * Parsing
    ParsedModule (..),
    ParseError (..),
    describeParseError,
    parseText,

    -- * Options
    ParserConfig (..),
    defaultParserConfig,
    sourceExtensions,
  )
where

import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Driver.Session qualified as GHC
import Data.Text qualified as T
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer qualified as GHC
import GHC.Hs (HsModule (..))
import GHC.Hs.Extension (GhcPs)
import GHC.LanguageExtensions.Type (Extension)
import GHC.Parser qualified as GHC
import GHC.Parser.Annotation (getLocA)
import GHC.Parser.Lexer qualified as GHC
import GHC.Types.SrcLoc qualified as GHC
import GHC.Unit.Module.Warnings (emptyWarningCategorySet)
import GHC.Utils.Error qualified as GHC
import GHC.Utils.Outputable qualified as GHC
import Tilia.Comments (Comment, commentsOf)
import Tilia.Span (Span (..))
import Tilia.Span.Ghc (spanOfReal)

-- | A module that parsed, together with the comments found in it.
data ParsedModule = ParsedModule
  { -- | The syntax tree, exactly as GHC produced it.
    pmModule :: HsModule GhcPs,
    -- | Every comment in the module, in source order.
    --
    -- Haddocks are here too. GHC also puts them in the syntax tree, but
    -- reconstructing one from what it puts there cannot reproduce what the
    -- author wrote, so the text is taken from the comment stream and the
    -- tree is used only to know which comments are Haddocks.
    pmComments :: [Comment],
    -- | Where the file header stops and the module proper begins, if the
    -- module has anything after its header.
    --
    -- GHC reads pragmas from the header and nowhere else, so this is the
    -- line that decides whether a @{-# … #-}@ is a pragma at all. One
    -- written below it has no effect on compilation, and hoisting it to the
    -- top of the file would give it one.
    pmHeaderEnd :: Maybe Span
  }

-- | Why a module did not parse.
newtype ParseError = ParseError
  { -- | Where the parser gave up.
    peSpan :: GHC.SrcSpan
  }

-- | Where the parser gave up, in a form fit to show.
--
-- Callers should not have to depend on the compiler's libraries merely to
-- report that a file did not parse, which is what turning the span into
-- text here is for.
describeParseError :: ParseError -> Text
describeParseError = T.pack . GHC.showSDocUnsafe . GHC.ppr . peSpan

-- | What the parser is allowed to accept.
newtype ParserConfig = ParserConfig
  { -- | Extensions to enable before parsing.
    --
    -- These come from the module's own @LANGUAGE@ pragmas and from the
    -- @default-extensions@ of the package it belongs to; working out which
    -- is not this module's business.
    pcExtensions :: [Extension]
  }

-- | No extensions beyond whatever @GHC2021@ implies.
--
-- The edition has to be asked for explicitly: the parser is told a set of
-- extensions and knows nothing about editions, so a module relying on one
-- being in force—@foreign import@, say, which @ForeignFunctionInterface@
-- allows and nothing else does—would not parse without this.
defaultParserConfig :: ParserConfig
defaultParserConfig =
  ParserConfig {pcExtensions = GHC.languageExtensions (Just GHC.GHC2021)}

-- | Parse a module.
parseText ::
  ParserConfig ->
  -- | Path, used only in positions reported back
  FilePath ->
  -- | The source
  Text ->
  Either ParseError ParsedModule
parseText config path source =
  case GHC.unP GHC.parseModule initialState of
    GHC.PFailed pstate ->
      Left (ParseError {peSpan = GHC.mkSrcSpanPs (GHC.last_loc pstate)})
    GHC.POk pstate (GHC.L _ hsModule)
      | not (GHC.isEmptyMessages (GHC.getPsErrorMessages pstate)) ->
          Left (ParseError {peSpan = GHC.mkSrcSpanPs (GHC.last_loc pstate)})
      | otherwise ->
          Right
            ParsedModule
              { pmModule = hsModule,
                pmComments = commentsOf source hsModule,
                pmHeaderEnd = headerEndOf hsModule
              }
  where
    config' =
      config
        { pcExtensions = withImplied (pcExtensions config <> sourceExtensions source)
        }

    initialState =
      GHC.initParserState
        (parserOpts config')
        (GHC.stringToStringBuffer (T.unpack source))
        (GHC.mkRealSrcLoc (mkFastString path) 1 1)

-- | Close a set of extensions under what they imply.
--
-- @TemplateHaskell@ turns @TemplateHaskellQuotes@ on, and the lexer
-- consults the second rather than the first, so a module that asks only for
-- the first would not lex its own quotations. Implications that turn
-- something /off/ are ignored: a formatter wants to accept as much as it
-- can, and an extension left on that the compiler would have switched off
-- costs nothing here.
withImplied :: [Extension] -> [Extension]
withImplied = settle . nub
  where
    settle es =
      let es' = nub (es <> concatMap implied es)
       in if length es' == length es then es else settle es'
    implied e = [to | (from, GHC.On to) <- GHC.impliedXFlags, from == e]

-- | Options to parse with.
parserOpts :: ParserConfig -> GHC.ParserOpts
parserOpts ParserConfig {pcExtensions} =
  GHC.mkParserOpts
    (EnumSet.fromList pcExtensions)
    quietDiagnostics
    False -- safe imports
    True -- keep Haddock tokens
    True -- keep ordinary comment tokens
    True -- let pragmas move the source position

-- | Diagnostics are not reported, so the settings only have to be
-- well-formed.
quietDiagnostics :: GHC.DiagOpts
quietDiagnostics =
  GHC.DiagOpts
    { GHC.diag_warning_flags = EnumSet.empty,
      GHC.diag_fatal_warning_flags = EnumSet.empty,
      GHC.diag_custom_warning_categories = emptyWarningCategorySet,
      GHC.diag_fatal_custom_warning_categories = emptyWarningCategorySet,
      GHC.diag_warn_is_error = False,
      GHC.diag_reverse_errors = False,
      GHC.diag_max_errors = Nothing,
      GHC.diag_ppr_ctx = GHC.defaultSDocContext
    }

-- | The start of the first thing that is not part of the header.
--
-- Imports and declarations are the only things that can end a header, and
-- either may come first, so both are consulted.
headerEndOf :: HsModule GhcPs -> Maybe Span
headerEndOf hsModule =
  foldl' earliest Nothing $
    map getLocA (hsmodImports hsModule)
      <> map getLocA (hsmodDecls hsModule)
  where
    earliest acc l = case GHC.srcSpanToRealSrcSpan l of
      Nothing -> acc
      Just s ->
        let this = spanOfReal s
         in Just (maybe this (keepEarlier this) acc)
    keepEarlier a b
      | (spanStartLine a, spanStartColumn a) <= (spanStartLine b, spanStartColumn b) = a
      | otherwise = b

----------------------------------------------------------------------------
-- Language pragmas

-- | The extensions a module's own @LANGUAGE@ pragmas ask for.
--
-- A @No@-prefixed name turns one off, so it is dropped from the result
-- rather than added; nothing here is enabled by default, so removing what
-- was never added is a no-op and that is correct.
--
-- Names GHC does not know are ignored. A module asking for an extension
-- this compiler has never heard of will not parse anyway, and failing to
-- parse is a better answer than refusing to try.
sourceExtensions :: Text -> [Extension]
sourceExtensions = foldl' apply [] . concatMap pragmaNames . headerLines
  where
    apply acc name = case T.stripPrefix "No" name >>= lookupExtension of
      Just off -> filter (/= off) acc
      Nothing -> case lookupExtension name of
        Just on | on `notElem` acc -> acc <> [on]
        _ -> acc
    headerLines = T.lines
    pragmaNames l = case T.stripPrefix "{-#" (T.stripStart l) of
      Nothing -> []
      Just rest ->
        let body = T.takeWhile (/= '#') rest
            (keyword, names) = T.break (== ' ') (T.stripStart body)
         in if T.toUpper keyword == "LANGUAGE"
              then filter (not . T.null) (map T.strip (T.splitOn "," names))
              else []

lookupExtension :: Text -> Maybe Extension
lookupExtension name = Map.lookup name extensionsByName

-- | Every extension this compiler knows, by the name one writes in a
-- pragma.
extensionsByName :: Map Text Extension
extensionsByName =
  Map.fromList [(T.pack (show e), e) | e <- [minBound .. maxBound]]
