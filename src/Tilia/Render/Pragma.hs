{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

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
    pragmaBrackets,
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
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import GHC.Unit.Module.Warnings
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Render.Name

----------------------------------------------------------------------------
-- Braces

-- | Wrap a body in pragma braces.
--
-- The closing brace is indented when the pragma breaks, which keeps it from
-- being mistaken for the start of a new declaration.
pragmaBrackets :: Doc -> Doc
pragmaBrackets body =
  align (txt "{-#" <> space <> body <> breakOrSpace <> indent (txt "#-}"))

-- | A named pragma with a body.
pragma :: Text -> Doc -> Doc
pragma pragmaName body =
  pragmaBrackets (txt pragmaName <> breakOrSpace <> body)

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
    -- Written out whole rather than built with 'pragmaBrackets': an overlap
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
warnDecls ctx (Warnings _ warnings) = case warnings of
  [] -> mempty
  (L _ (Warning _ _ wtxt) : _) ->
    layoutAcross ctx warnings
      . pragma (keywordOf wtxt)
      . indent
      $ sepBy (txt ";" <> breakOrSpace) (map (at_ ctx (warned ctx)) warnings)
  where
    keywordOf wtxt = let (keyword, _, _) = warningParts wtxt in keyword

-- | One of the things a warning declaration names.
warned :: Ctx -> WarnDecl GhcPs -> Doc
warned ctx (Warning (namespace, _) names wtxt) =
  category
    <> namespaceSpec namespace
    <> commaSep (map (name ctx) names)
    <> breakOrSpace
    <> literalList literals
  where
    (_, category, literals) = warningParts wtxt

-- | A warning attached to a name in an export list or to an instance.
warningTxt :: WarningTxt GhcPs -> Doc
warningTxt wtxt =
  indent (pragma keyword (indent (category <> literalList literals)))
  where
    (keyword, category, literals) = warningParts wtxt

-- | Which keyword introduces a warning, which category it is filed under,
-- and what it says.
--
-- The keyword is written once for a whole declaration even when it names
-- several things, whereas the category belongs to each of them separately.
-- That is why the two do not come back as one piece of text.
warningParts :: WarningTxt GhcPs -> (Text, Doc, [LocatedE StringLiteral])
warningParts = \case
  DeprecatedTxt _ literals -> ("DEPRECATED", mempty, said literals)
  WarningTxt category _ literals ->
    ("WARNING", foldMap named category, said literals)
  where
    said = map (fmap hsDocString)
    named (unLoc -> InWarningCategory {..}) =
      txt ("in \"" <> showGhc (unLoc iwc_wc) <> "\"") <> space

-- | One message is written bare; several go in a list.
literalList :: [LocatedE StringLiteral] -> Doc
literalList = \case
  [l] -> outputable l
  ls -> brackets (commaSep (map outputable ls))
