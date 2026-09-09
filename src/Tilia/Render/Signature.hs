{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Signatures, and the pragmas that are written like them.
module Tilia.Render.Signature
  ( sigDecl,
    standaloneKindSig,
    ruleDecls,
    specialisedName,
  )
where

import Data.Maybe (maybeToList)
import GHC.Data.BooleanFormula
import GHC.Hs
import GHC.Types.Basic
  ( Activation (..),
    InlinePragma (..),
    InlineSpec (..),
    RuleMatchInfo (..),
    RuleName,
  )
import GHC.Types.Fixity (Fixity (..), FixityDirection (..))
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.SourceText
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Render.Expression (hsExpr)
import Tilia.Render.Name
import Tilia.Render.Pragma
import Tilia.Render.Type
import Tilia.Span (startOf)
import Tilia.Span.Ghc (tokenSpan)

-- | A signature declaration.
sigDecl :: Ctx -> Sig GhcPs -> Doc
sigDecl ctx = \case
  TypeSig _ names hswc -> typeSig ctx True names (hswc_body hswc)
  PatSynSig _ names sigType -> patSynSig ctx names sigType
  ClassOpSig _ isDefault names sigType ->
    includeWhen isDefault (txt "default" <> space) <> typeSig ctx True names sigType
  FixSig _ sig -> fixitySig ctx sig
  InlineSig _ n prag -> inlineSig ctx n prag
  SpecSig _ n types prag ->
    specialiseSig ctx Nothing (noLocA (HsVar NoExtField n)) types prag
  SpecSigE _ binders e prag -> specialiseSigE ctx binders e prag
  SpecInstSig _ sigType ->
    pragma "SPECIALIZE instance" (indent (hsSigType ctx sigType))
  MinimalSig _ formula ->
    at ctx formula (pragma "MINIMAL" . indent . booleanFormula ctx)
  CompleteMatchSig _ names ty -> completeSig ctx names ty
  SCCFunSig _ n literal -> sccSig ctx n literal

-- | @f, g :: t@.
--
-- Only the first name sits on the line the signature starts on; the rest
-- are indented under it, unless the caller says not to, which is what a
-- pattern synonym signature wants so that its names line up after
-- @pattern@.
typeSig ::
  Ctx ->
  -- | Indent the names after the first?
  Bool ->
  [LocatedN RdrName] ->
  LHsSigType GhcPs ->
  Doc
typeSig _ _ [] _ = mempty
typeSig ctx indentTail (n : ns) sigType
  | null ns = name ctx n <> typeAscription ctx sigType
  | otherwise =
      name ctx n
        <> nest
          (if indentTail then 1 else 0)
          ( comma
              <> breakOrSpace
              <> commaSep (map (name ctx) ns)
              <> typeAscription ctx sigType
          )

patSynSig :: Ctx -> [LocatedN RdrName] -> LHsSigType GhcPs -> Doc
patSynSig ctx names sigType
  | length names > 1 = txt "pattern" <> breakOrSpace <> indent body
  | otherwise = txt "pattern" <> space <> body
  where
    body = typeSig ctx False names sigType

fixitySig :: Ctx -> FixitySig GhcPs -> Doc
fixitySig ctx (FixitySig namespace names (Fixity precedence direction)) =
  txt keyword
    <> space
    <> outputable precedence
    <> space
    <> namespaceSpec namespace
    <> align (commaSep (map (name ctx) names))
  where
    keyword = case direction of
      InfixL -> "infixl"
      InfixR -> "infixr"
      InfixN -> "infix"

inlineSig :: Ctx -> LocatedN RdrName -> InlinePragma -> Doc
inlineSig ctx n InlinePragma {..} =
  pragmaBrackets $
    inlineSpec inl_inline
      <> space
      <> conLike
      <> space
      <> includeUnless (inl_act == NeverActive) (activation inl_act)
      <> space
      <> name ctx n
  where
    conLike = case inl_rule of
      ConLike -> txt "CONLIKE"
      FunLike -> mempty

-- | A @SPECIALIZE@ pragma.
specialiseSig ::
  Ctx ->
  Maybe (RuleBndrs GhcPs) ->
  LHsExpr GhcPs ->
  [LHsSigType GhcPs] ->
  InlinePragma ->
  Doc
specialiseSig ctx binders target types InlinePragma {..} =
  pragmaBrackets $
    txt "SPECIALIZE"
      <> space
      <> inlineSpec inl_inline
      <> space
      <> phase
      <> indent
        ( space
            <> foldMap (\bs -> ruleBinders ctx bs <> space) binders
            <> hsExpr ctx target
            <> includeUnless
              (null types)
              (joinedBy "::" <> commaSep (map (hsSigType ctx) types))
        )
  where
    -- A pragma that says neither when to inline nor whether to is saying
    -- nothing, so the phase is left off rather than printed as @[~]@.
    phase = case (inl_inline, inl_act) of
      (NoInline _, NeverActive) -> mempty
      _ -> activation inl_act

specialiseSigE ::
  Ctx ->
  RuleBndrs GhcPs ->
  LHsExpr GhcPs ->
  InlinePragma ->
  Doc
specialiseSigE ctx binders e =
  specialiseSig ctx (Just binders) target (maybeToList sigTy)
  where
    (_, target, sigTy) = takeApartSpecExpr e

-- | Pull a @SPECIALIZE@ expression apart into the name, the application and
-- the signature.
--
-- The expression in this position can only be a variable applied to
-- arguments, optionally with a type ascription, so the name is always
-- reachable by walking down the left spine.
takeApartSpecExpr ::
  LHsExpr GhcPs ->
  (LocatedN RdrName, LHsExpr GhcPs, Maybe (LHsSigType GhcPs))
takeApartSpecExpr expr = (specHead applied, applied, signature)
  where
    (applied, signature) = specBody expr

-- | A @SPECIALIZE@ expression without the type ascription it may carry.
--
-- Everything else here works on the expression under the ascription: that
-- is what the pragma is about, and the ascription is printed separately.
specBody :: LHsExpr GhcPs -> (LHsExpr GhcPs, Maybe (LHsSigType GhcPs))
specBody = \case
  L _ (ExprWithTySig _ e HsWC {hswc_body}) -> (e, Just hswc_body)
  e -> (e, Nothing)

-- | The function a @SPECIALIZE@ expression applies.
--
-- Whatever else the expression does, it is an application, and the pragma
-- names whatever sits at the head of it.
specHead :: LHsExpr GhcPs -> LocatedN RdrName
specHead (L _ e) = case e of
  HsVar _ n -> n
  HsApp _ f _ -> specHead f
  HsAppType _ f _ -> specHead f
  _ -> error "Tilia: a SPECIALIZE expression always has a head variable"

-- | The name a @SPECIALIZE@ pragma is about, for grouping declarations.
specialisedName :: Sig GhcPs -> Maybe RdrName
specialisedName = \case
  SpecSig _ (L _ n) _ _ -> Just n
  SpecSigE _ _ e _ -> Just (unLoc (specHead (fst (specBody e))))
  _ -> Nothing

booleanFormula :: Ctx -> BooleanFormula GhcPs -> Doc
booleanFormula ctx = \case
  Var n -> name ctx n
  And xs -> align (commaSep (map (at_ ctx (booleanFormula ctx)) xs))
  Or xs ->
    align (sepBy (breakOrSpace <> txt "|" <> space) (map (at_ ctx (booleanFormula ctx)) xs))
  Parens l -> at ctx l (parens . booleanFormula ctx)

completeSig :: Ctx -> [LIdP GhcPs] -> Maybe (LocatedN RdrName) -> Doc
completeSig ctx names ty =
  layoutAcross ctx names . pragma "COMPLETE" . indent $
    commaSep (map (name ctx) names)
      <> foldMap
        (\t -> joinedBy "::" <> indent (name ctx t))
        ty

sccSig :: Ctx -> LocatedN RdrName -> Maybe (XRec GhcPs StringLiteral) -> Doc
sccSig ctx n literal =
  pragma "SCC" . indent $
    name ctx n <> foldMap (\l -> breakOrSpace <> outputable l) literal

-- | @type T :: k@.
standaloneKindSig :: Ctx -> StandaloneKindSig GhcPs -> Doc
standaloneKindSig ctx (StandaloneKindSig _ n sigTy) =
  txt "type"
    <> indent
      ( space
          <> name ctx n
          <> joinedBy "::"
          <> hsSigType ctx sigTy
      )

----------------------------------------------------------------------------
-- Rewrite rules

-- | A @RULES@ block.
--
-- The closing @#-\}@ is given an anchor of its own, so that a comment
-- written after the last rule and before it stays inside the pragma. There
-- is nothing else down there for such a comment to attach to, and outside
-- the braces it would read as a remark on whatever follows the block.
ruleDecls :: Ctx -> RuleDecls GhcPs -> Doc
ruleDecls ctx (HsRules ((_, close), _) rules) =
  pragma "RULES" $
    sepBy breakOrSpace (map (align . at_ ctx (ruleDecl ctx)) rules)
      <> foldMap (emptyAnchor . startOf) (tokenSpan close)

ruleDecl :: Ctx -> RuleDecl GhcPs -> Doc
ruleDecl ctx (HsRule _ ruleName phase binders lhs rhs) =
  at ctx ruleName ruleNameLiteral
    <> space
    <> activation phase
    <> space
    <> ruleBinders ctx binders
    <> breakOrSpace
    <> indent
      ( hsExpr ctx lhs
          <> space
          <> txt "="
          <> indent (breakOrSpace <> hsExpr ctx rhs)
      )

-- | A rule's name is a string literal, and printing it as one is what puts
-- the quotes back.
ruleNameLiteral :: RuleName -> Doc
ruleNameLiteral n = outputable (HsString NoSourceText n :: HsLit GhcPs)

-- | The @forall@s a rule or a @SPECIALIZE@ pragma binds.
ruleBinders :: Ctx -> RuleBndrs GhcPs -> Doc
ruleBinders ctx (RuleBndrs HsRuleBndrsAnn {..} tyvars binders) =
  foldMap
    (\xs -> forallBndrs ctx Invisible (tyVarBndr ctx) xs <> space)
    tyvars
    <> case rb_tmanns of
      Nothing -> mempty
      Just _ -> forallBndrs ctx Invisible (ruleBinder ctx) binders

ruleBinder :: Ctx -> RuleBndr GhcPs -> Doc
ruleBinder ctx = \case
  RuleBndr _ n -> name ctx n
  RuleBndrSig _ n HsPS {..} ->
    parens (name ctx n <> typeAscription ctx (asSigType hsps_body))
