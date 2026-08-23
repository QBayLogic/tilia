{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The @{-# … #-}@ annotations that appear among declarations.
--
-- These look like comments and are not: the compiler reads them, so their
-- content is not ours to reflow and their placement is not ours to change.
-- What is ours is where the braces break, which is all this module decides.
--
-- The @LANGUAGE@ and @OPTIONS_GHC@ pragmas of the file header are not here.
-- They are part of the header rather than of any declaration, they are
-- sorted rather than left where they were, and they are handled in
-- "Tilia.Render.Header".
module Tilia.Render.Pragma
  ( -- * Braces
    pragmaBraces,
    pragma,

    -- * Inlining and rules
    activation,
    inlineSpec,

    -- * Instances
    overlapMode,

    -- * Warnings
    warnDecls,
    warningTxt,
  )
where

import Data.Text (Text)
import GHC.Hs
import GHC.Types.Basic hiding (overlapMode)
import GHC.Types.SourceText
import GHC.Types.SrcLoc (unLoc)
import GHC.Unit.Module.Warnings
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Render.Name
import Tilia.Span.Ghc

----------------------------------------------------------------------------
-- Braces

-- | Wrap a body in pragma braces.
--
-- The closing brace is indented when the pragma breaks, which keeps it from
-- being mistaken for the start of a new declaration.
pragmaBraces :: Doc -> Doc
pragmaBraces body =
  align (txt "{-#" <> space <> body <> breakOrSpace <> indent (txt "#-}"))

-- | A named pragma with a body.
pragma :: Text -> Doc -> Doc
pragma pragmaName body =
  pragmaBraces (txt pragmaName <> breakOrSpace <> body)

----------------------------------------------------------------------------
-- Inlining and rules

-- | The phase control of an @INLINE@ or @RULES@ pragma.
activation :: Activation -> Doc
activation = \case
  NeverActive -> txt "[~]"
  AlwaysActive -> mempty
  ActiveBefore _ n -> txt "[~" <> outputable n <> txt "]"
  ActiveAfter _ n -> txt "[" <> outputable n <> txt "]"
  FinalActive -> error "Tilia: FinalActive is not expected in parsed source"

-- | Which flavour of inlining was asked for.
inlineSpec :: InlineSpec -> Doc
inlineSpec = \case
  Inline _ -> txt "INLINE"
  Inlinable _ -> txt "INLINEABLE"
  NoInline _ -> txt "NOINLINE"
  Opaque _ -> txt "OPAQUE"
  NoUserInlinePrag -> mempty

----------------------------------------------------------------------------
-- Instances

-- | The overlap pragma of an instance, and the separator after it.
overlapMode :: Maybe (LocatedP OverlapMode) -> Maybe Doc
overlapMode mode = txt . braced <$> (spelled . unLoc =<< mode)
  where
    -- Written out whole rather than built with 'pragmaBraces': an overlap
    -- mode is one word and must never be broken across lines.
    braced keyword = "{-# " <> keyword <> " #-}"

    spelled = \case
      Overlappable {} -> Just "OVERLAPPABLE"
      Overlapping {} -> Just "OVERLAPPING"
      Overlaps {} -> Just "OVERLAPS"
      Incoherent {} -> Just "INCOHERENT"
      -- The rest are what an instance means when it says nothing about
      -- overlapping, so nothing is what they are written as.
      _ -> Nothing

----------------------------------------------------------------------------
-- Warnings

-- | A @WARNING@ or @DEPRECATED@ declaration.
warnDecls :: Ctx -> WarnDecls GhcPs -> Doc
warnDecls ctx (Warnings _ warnings) =
  vsep (map (at_ ctx (warnDecl ctx)) warnings)

warnDecl :: Ctx -> WarnDecl GhcPs -> Doc
warnDecl ctx (Warning (namespace, _) names wtxt) =
  layoutFrom ctx (spansOf names <> spansOf literals) $
    pragma pragmaName . indent $
      namespaceSpec namespace
        <> commaSep (map (name ctx) names)
        <> breakOrSpace
        <> literalList literals
  where
    (pragmaName, literals) = warningParts wtxt

-- | A warning attached to a name in an export list or to an instance.
warningTxt :: WarningTxt GhcPs -> Doc
warningTxt wtxt =
  indent (pragma pragmaName (indent (literalList literals)))
  where
    (pragmaName, literals) = warningParts wtxt

warningParts :: WarningTxt GhcPs -> (Text, [LocatedE StringLiteral])
warningParts w = (keyword, fmap hsDocString <$> messages)
  where
    (keyword, messages) = case w of
      DeprecatedTxt _ literals -> ("DEPRECATED", literals)
      WarningTxt category _ literals ->
        ("WARNING" <> foldMap named category, literals)

    -- A warning may be filed under a category, which is written in quotes
    -- after the keyword.
    named (unLoc -> InWarningCategory {..}) =
      " in \"" <> showGhc (unLoc iwc_wc) <> "\""

-- | One message is written bare; several go in a list.
literalList :: [LocatedE StringLiteral] -> Doc
literalList = \case
  [l] -> outputable l
  ls -> brackets (commaSep (map outputable ls))
