{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Turning source text into a syntax tree and a comment stream.
module Linden.Parser
  ( -- * Parsing
    ParsedModule (..),
    ParseError (..),
    parseText,

    -- * Options
    ParserConfig (..),
    defaultParserConfig,
  )
where

import Data.Text (Text)
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
import GHC.Utils.Error qualified as GHC
import GHC.Unit.Module.Warnings (emptyWarningCategorySet)
import GHC.Utils.Outputable qualified as GHC
import Linden.Comments (Comment, commentsOf)
import Linden.Printer.Combinators (Span (..), mkSpan)

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
defaultParserConfig :: ParserConfig
defaultParserConfig = ParserConfig {pcExtensions = []}

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
    GHC.POk _ (GHC.L _ hsModule) ->
      Right
        ParsedModule
          { pmModule = hsModule,
            pmComments = commentsOf source hsModule,
            pmHeaderEnd = headerEndOf hsModule
          }
  where
    initialState =
      GHC.initParserState
        (parserOpts config)
        (GHC.stringToStringBuffer (T.unpack source))
        (GHC.mkRealSrcLoc (mkFastString path) 1 1)

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
        let this = toSpan s
         in Just (maybe this (keepEarlier this) acc)
    keepEarlier a b
      | (spanStartLine a, spanStartColumn a) <= (spanStartLine b, spanStartColumn b) = a
      | otherwise = b

-- | Convert a GHC span to the printer's.
toSpan :: GHC.RealSrcSpan -> Span
toSpan s =
  mkSpan
    (GHC.srcSpanStartLine s, GHC.srcSpanStartCol s)
    (GHC.srcSpanEndLine s, GHC.srcSpanEndCol s)
