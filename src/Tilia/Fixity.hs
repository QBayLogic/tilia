{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Working out the fixity of the operators a module uses.
module Tilia.Fixity
  ( -- * Fixities
    OpName (..),
    Direction (..),
    Fixity (..),
    defaultFixity,

    -- * What a module declares
    declaredFixities,
    declaredNames,
    moduleName,

    -- * What a module passes on
    ExportItem (..),
    moduleExports,

    -- * What a module can see
    Import (..),
    moduleImports,
    Scope (..),
    resolveScope,

    -- * Answers
    Provenance (..),
    Resolution (..),
    lookupFixity,
    unreadFor,

    -- * What could not be answered
    Unknown (..),
    operatorsUsed,
    unknownOperators,
    operatorSpelling,

    -- * What reading a module established
    Established (..),
  )
where

import Data.Foldable (toList)
import Data.Generics.Schemes (listify)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs hiding (Fixity, OpName)
import GHC.Types.Fixity qualified as GHC
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName (..), rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), unLoc)

----------------------------------------------------------------------------
-- Fixities

-- | An operator, spelled as it appears in an @infix@ declaration: @<+>@, or
-- @div@ for a function used infix in backticks.
newtype OpName = OpName Text
  deriving (Eq, Ord, Show)

-- | Which way an operator associates.
data Direction = LeftAssoc | RightAssoc | NoAssoc
  deriving (Eq, Show)

-- | A fixity: how tightly an operator binds, and which way it associates.
data Fixity = Fixity
  { fixityDirection :: Direction,
    fixityPrecedence :: Int
  }
  deriving (Eq, Show)

-- | What an operator with no declaration in scope means: @infixl 9@.
defaultFixity :: Fixity
defaultFixity = Fixity LeftAssoc 9

----------------------------------------------------------------------------
-- What a module declares

-- | The fixities a module declares for its own operators.
declaredFixities :: HsModule GhcPs -> Map OpName Fixity
declaredFixities =
  Map.fromList . concatMap (fromDecl . unLoc) . hsmodDecls
  where
    fromDecl = \case
      SigD _ sig -> fromSig sig
      TyClD _ ClassDecl {tcdSigs} -> concatMap (fromSig . unLoc) tcdSigs
      _ -> []
    fromSig = \case
      FixSig _ (FixitySig _ names fixity) ->
        [(opName (unLoc n), fromGhcFixity fixity) | n <- names]
      _ -> []

-- | Every name a module defines itself.
--
-- Not the same question as 'declaredFixities', which is about @infix@
-- declarations. This one is asked of an export list: a name a module
-- exports and also defines needs no chasing, and one it merely passes on
-- does. Getting the two confused makes a module appear to re-export
-- everything it exports, and then a single dependency whose source is
-- missing makes the whole module unanswerable.
--
-- Erring towards too few is safe and towards too many is not: a name left
-- out here is chased when it need not have been, whereas one wrongly
-- included is a fixity nobody looked for.
declaredNames :: HsModule GhcPs -> Set OpName
declaredNames = Set.fromList . concatMap (fromDecl . unLoc) . hsmodDecls
  where
    fromDecl = \case
      ValD _ b -> fromBind b
      SigD _ sig -> fromSig sig
      TyClD _ t -> fromTyCl t
      ForD _ f -> [opName (unLoc (fd_name f))]
      _ -> []

    fromBind = \case
      FunBind _ n _ -> [opName (unLoc n)]
      PatBind _ p _ _ -> boundByPattern p
      PatSynBind _ (PSB _ n _ _ _) -> [opName (unLoc n)]
      _ -> []

    fromSig = \case
      TypeSig _ ns _ -> map (opName . unLoc) ns
      ClassOpSig _ _ ns _ -> map (opName . unLoc) ns
      PatSynSig _ ns _ -> map (opName . unLoc) ns
      FixSig _ (FixitySig _ ns _) -> map (opName . unLoc) ns
      _ -> []

    fromTyCl = \case
      FamDecl _ (FamilyDecl {fdLName}) -> [opName (unLoc fdLName)]
      SynDecl {tcdLName} -> [opName (unLoc tcdLName)]
      DataDecl {tcdLName, tcdDataDefn} ->
        opName (unLoc tcdLName) : concatMap (fromCon . unLoc) (consOf (dd_cons tcdDataDefn))
      ClassDecl {tcdLName, tcdSigs} ->
        opName (unLoc tcdLName) : concatMap (fromSig . unLoc) tcdSigs

    consOf :: DataDefnCons (LConDecl GhcPs) -> [LConDecl GhcPs]
    consOf = toList

    fromCon :: ConDecl GhcPs -> [OpName]
    fromCon = \case
      ConDeclGADT {con_names} -> map (opName . unLoc) (toList con_names)
      ConDeclH98 {con_name, con_args} ->
        opName (unLoc con_name) : fieldNames con_args

    -- A record field is a name the module defines too, and it may be an
    -- operator.
    fieldNames :: HsConDeclH98Details GhcPs -> [OpName]
    fieldNames = \case
      RecCon fields ->
        [ opName (unLoc (foLabel (unLoc n)))
        | f <- unLoc fields,
          n <- cdrf_names (unLoc f)
        ]
      _ -> []

    -- A pattern binding brings in whatever its variables name.
    boundByPattern p =
      [opName n | VarPat _ (L _ n) <- listify isVarPat p]
    isVarPat :: Pat GhcPs -> Bool
    isVarPat = \case
      VarPat {} -> True
      _ -> False

-- | Render a parsed name as an operator name.
opName :: RdrName -> OpName
opName = OpName . T.pack . occNameString . rdrNameOcc

fromGhcFixity :: GHC.Fixity -> Fixity
fromGhcFixity (GHC.Fixity prec dir) = Fixity (fromGhcDirection dir) prec

fromGhcDirection :: GHC.FixityDirection -> Direction
fromGhcDirection = \case
  GHC.InfixL -> LeftAssoc
  GHC.InfixR -> RightAssoc
  GHC.InfixN -> NoAssoc

-- | The module's own name, if it declares one.
moduleName :: HsModule GhcPs -> Maybe Text
moduleName = fmap (T.pack . moduleNameString . unLoc) . hsmodName

----------------------------------------------------------------------------
-- What a module passes on

-- | One entry of a module's export list.
data ExportItem
  = -- | A name, which may or may not be declared in this module, under the
    -- qualifier it was written with if it was written with one.
    ExportName (Maybe Text) OpName
  | -- | @module M@, re-exporting everything that module brought in.
    ExportModule Text
  deriving (Eq, Show)

-- | The qualifier a name was written under.
qualifierOf :: RdrName -> Maybe Text
qualifierOf = \case
  Qual m _ -> Just (T.pack (moduleNameString m))
  _ -> Nothing

-- | A module's export list, or 'Nothing' if it has none.
--
-- The distinction matters. A module with no export list exports exactly
-- what it defines, so its own declarations are the whole answer. A module
-- with one may be passing on names it never declared, and those are what
-- re-export resolution has to chase.
moduleExports :: HsModule GhcPs -> Maybe [ExportItem]
moduleExports =
  fmap (concatMap (fromIE . unLoc) . unLoc) . hsmodExports
  where
    fromIE = \case
      IEVar _ n _ -> [named n]
      IEThingAbs _ n _ -> [named n]
      IEThingAll _ n _ -> [named n]
      -- The type itself and every member listed with it; a class exports
      -- its operators this way.
      IEThingWith _ n _ ns _ -> named n : map named ns
      IEModuleContents _ m -> [ExportModule (T.pack (moduleNameString (unLoc m)))]
      _ -> []
    named n =
      let rdr = ieWrappedName (unLoc n)
       in ExportName (qualifierOf rdr) (opName rdr)

----------------------------------------------------------------------------
-- What a module can see

-- | One import declaration, reduced to what bears on fixity.
data Import = Import
  { -- | The module being imported.
    importModule :: Text,
    -- | Whether unqualified names are brought into scope. An import that is
    -- @qualified@ brings none.
    importQualified :: Bool,
    -- | The name qualified uses go through: the alias if there is one,
    -- otherwise the module's own name.
    importAlias :: Text,
    -- | The explicit list, if there is one, and whether it is a @hiding@
    -- list. Only operators are kept; nothing else can carry a fixity.
    importNames :: Maybe (Bool, [OpName])
  }
  deriving (Eq, Show)

-- | The imports of a module.
--
-- @Prelude@ is added when the module does not name it itself. It is where
-- @($)@, @(.)@ and most of what an operator chain is made of are declared,
-- and a module that does not mention it still sees all of them. A module
-- compiled with @NoImplicitPrelude@ does not, but the extensions are not
-- visible here, and the cost of the mistake is one extra module consulted
-- for names the module is not using.
moduleImports :: HsModule GhcPs -> [Import]
moduleImports hsModule = implicitPrelude <> written
  where
    written = map (fromDecl . unLoc) (hsmodImports hsModule)
    implicitPrelude
      | any ((== "Prelude") . importModule) written = []
      | otherwise =
          [ Import
              { importModule = "Prelude",
                importQualified = False,
                importAlias = "Prelude",
                importNames = Nothing
              }
          ]

    fromDecl d =
      Import
        { importModule = modName (unLoc (ideclName d)),
          importQualified = ideclQualified d /= NotQualified,
          importAlias = maybe (modName (unLoc (ideclName d))) (modName . unLoc) (ideclAs d),
          importNames = fromList <$> ideclImportList d
        }
    fromList (interpretation, names) =
      ( interpretation == EverythingBut,
        mapMaybe (importedOp . unLoc) (unLoc names)
      )
    modName = T.pack . moduleNameString

-- | An operator mentioned in an import list.
--
-- A name is only interesting here if it could carry a fixity, which means
-- it has to be an operator. @IEThingWith@—a type with its constructors, as
-- in @Map(..)@—can bring operators in as well, and is not handled yet; see
-- the note on 'resolveScope'.
importedOp :: IE GhcPs -> Maybe OpName
importedOp = \case
  IEVar _ n _ -> Just (opName (ieWrappedName (unLoc n)))
  IEThingAbs _ n _ -> Just (opName (ieWrappedName (unLoc n)))
  _ -> Nothing

-- | Every fixity a module can see, and how.
data Scope = Scope
  { -- | Reachable without qualification, with where it came from.
    scopeUnqualified :: Map OpName (Fixity, Provenance),
    -- | Reachable as @M.op@, keyed by the alias actually written—or by the
    -- module's own name, under which its own declarations are reachable.
    scopeQualified :: Map (Text, OpName) (Fixity, Provenance),
    -- | The imports whose modules could not be read.
    --
    -- These are what separate \"no declaration exists\" from \"we did not
    -- manage to look\". An operator that was not found is settled only if no
    -- unread import could have brought it in, and deciding that needs the
    -- whole import rather than the module's name: see 'unreadFor'.
    scopeUnread :: [Import],
    -- | Operators the imports bring in with two different fixities, as they
    -- would have to be written to run into it: without a qualifier, or under
    -- the alias the disagreeing imports share.
    scopeAmbiguous :: [(Maybe Text, OpName)]
  }
  deriving (Eq, Show)

-- | Work out what a module can see.
--
-- The lookup function supplies what each imported module exports, and
-- 'Nothing' means it could not be determined—the package was not
-- downloaded, the source did not parse. That distinction is the whole point
-- of its type: an empty map is a fact about a module, whereas 'Nothing' is
-- an admission about us, and conflating them is how a formatter ends up
-- asserting a fixity it never established.
--
-- Not handled here: operators arriving through @T(..)@. That is syntactic
-- and so belongs to the lookup function, as re-export chains do—and those
-- "Tilia.Fixity.Plan" already follows, through export lists in source and
-- through the export section of an interface.
resolveScope ::
  -- | What a module exports, or 'Nothing' if that could not be determined
  (Text -> Maybe (Map OpName Fixity)) ->
  HsModule GhcPs ->
  Scope
resolveScope exportsOf hsModule =
  Scope
    { scopeUnqualified = Map.union own (Map.map fst unqualified),
      scopeQualified = qualified,
      scopeUnread = unread,
      scopeAmbiguous =
        [(Nothing, op) | op <- Map.keys (Map.filter snd unqualified)]
          <> [(Just alias, op) | (alias, op) <- Map.keys (Map.filter snd qualifiedFrom)]
    }
  where
    own = Map.map (,DeclaredHere) (declaredFixities hsModule)
    imports = moduleImports hsModule

    unread = [i | i <- imports, Nothing <- [exportsOf (importModule i)]]

    -- Paired with a flag saying whether two imports disagreed about it.
    unqualified =
      Map.unionsWith disagree
        [ Map.map (,False) (visible i)
        | i <- imports,
          not (importQualified i)
        ]
    disagree (a, aBad) (b, bBad) = (a, aBad || bBad || fst a /= fst b)

    qualified = Map.union ownQualified (Map.map fst qualifiedFrom)

    ownQualified =
      Map.fromList
        [ ((m, op), entry)
        | m <- toList (moduleName hsModule),
          (op, entry) <- Map.toList own
        ]

    qualifiedFrom =
      Map.unionsWith
        disagree
        [ Map.mapKeys (importAlias i,) (Map.map (,False) (visible i))
        | i <- imports
        ]

    visible i =
      let exported =
            Map.map (,DeclaredIn (importModule i)) $
              fromMaybe Map.empty (exportsOf (importModule i))
       in case importNames i of
            Nothing -> exported
            Just (True, hidden) -> Map.withoutKeys exported (setOf hidden)
            Just (False, shown) -> Map.restrictKeys exported (setOf shown)
    setOf = Map.keysSet . Map.fromList . map (,())

----------------------------------------------------------------------------
-- Answers

-- | Where a fixity came from.
--
-- Kept so that an answer can be explained, and so that
-- 'ReportDefault'—which is a real answer, not a guess—cannot be confused
-- with not having one.
data Provenance
  = -- | An @infix@ declaration in the module being formatted.
    DeclaredHere
  | -- | An @infix@ declaration in the named imported module.
    DeclaredIn Text
  | -- | No declaration exists anywhere in scope, and every module in scope
    -- was successfully consulted, so the Report's @infixl 9@ applies.
    ReportDefault
  deriving (Eq, Show)

-- | What is known about an operator at a use site.
data Resolution
  = -- | Established, and here is where from.
    Resolved Fixity Provenance
  | -- | Not established. The listed modules could not be read, and the
    -- answer may be in one of them.
    --
    -- A printer that receives this must not restructure the operator chain:
    -- it has to lay it out as the input had it. Rearranging on a guess is
    -- exactly what this type exists to prevent.
    Unresolved (NonEmpty Text)
  deriving (Eq, Show)

-- | The fixity of an operator as this module sees it.
lookupFixity ::
  -- | The scope
  Scope ->
  -- | The qualifier written at the use site, if any
  Maybe Text ->
  -- | Operator to resolve
  OpName ->
  -- | The resolution
  Resolution
lookupFixity scope qualifier op =
  case found of
    Just (fixity, provenance) -> Resolved fixity provenance
    Nothing -> case nonEmpty (unreadFor scope qualifier op) of
      Nothing -> Resolved defaultFixity ReportDefault
      Just missing -> Unresolved missing
  where
    found = case qualifier of
      Nothing -> Map.lookup op (scopeUnqualified scope)
      Just q -> Map.lookup (q, op) (scopeQualified scope)

-- | The modules of the unread imports that could have settled this use.
--
-- Empty means an operator that was not found really is undeclared, rather
-- than declared somewhere we failed to look. Getting this narrow matters:
-- an import that is @qualified as M@ has no bearing on an operator written
-- without a qualifier, and one with an import list has none on an operator
-- the list does not name. Were every unread import to count against every
-- operator, one unreachable package deep in a dependency tree would
-- unsettle a whole file.
unreadFor ::
  -- | The scope
  Scope ->
  -- | The qualifier written at the use site, if any
  Maybe Text ->
  -- | Operator being resolved
  OpName ->
  -- | The modules that could hold the answer
  [Text]
unreadFor scope qualifier op =
  [importModule i | i <- scopeUnread scope, reaches i, brings i]
  where
    reaches i = case qualifier of
      Nothing -> not (importQualified i)
      Just q -> q == importAlias i
    brings i = case importNames i of
      Nothing -> True
      Just (True, hidden) -> op `notElem` hidden
      Just (False, shown) -> op `elem` shown

----------------------------------------------------------------------------
-- What could not be answered

-- | Why an operator's fixity could not be settled.
data Unknown
  = -- | These modules in scope could not be read, and the declaration the
    -- answer depends on may be in any of them.
    NotRead (NonEmpty Text)
  | -- | Two modules in scope bring it in with different fixities, so which
    -- one applies cannot be read off the imports alone.
    Ambiguous
  deriving (Eq, Show)

-- | Every operator the module uses where its fixity decides the layout.
--
-- Only these positions. An operator chain in an expression and one in a type
-- are regrouped by precedence, so getting the precedence wrong changes what
-- the code means. Everywhere else—a section, the left-hand side of a
-- definition, an @infix@ declaration—the operator stands on its own and
-- nothing is regrouped around it.
operatorsUsed :: HsModule GhcPs -> [(Maybe Text, OpName)]
operatorsUsed hsModule = map named (inExpressions <> inTypes)
  where
    inExpressions =
      [ n
        | e :: HsExpr GhcPs <- listify (const True) hsModule,
          OpApp _ _ op _ <- [e],
          HsVar _ (L _ n) <- [unLoc op]
      ]
    inTypes =
      [ n
        | t :: HsType GhcPs <- listify (const True) hsModule,
          HsOpTy _ _ _ (L _ n) _ <- [t]
      ]
    named n = (qualifierOf n, OpName (T.pack (occNameString (rdrNameOcc n))))

-- | The operators this module uses that the scope cannot settle, as the
-- module writes them.
--
-- Empty is the only acceptable answer: an operator whose fixity is not
-- known cannot be laid out, only guessed at.
unknownOperators :: Scope -> HsModule GhcPs -> [((Maybe Text, OpName), Unknown)]
unknownOperators scope hsModule =
  Map.toList (Map.fromList (mapMaybe unsettled (operatorsUsed hsModule)))
  where
    ambiguous = Set.fromList (scopeAmbiguous scope)
    unsettled (qualifier, op) = case lookupFixity scope qualifier op of
      Unresolved missing -> Just ((qualifier, op), NotRead missing)
      Resolved _ _
        | Set.member (qualifier, op) ambiguous -> Just ((qualifier, op), Ambiguous)
        | otherwise -> Nothing

-- | An operator as a use site writes it, qualifier and all.
operatorSpelling :: Maybe Text -> OpName -> Text
operatorSpelling qualifier (OpName op) = maybe "" (<> ".") qualifier <> op

----------------------------------------------------------------------------
-- What reading a module established

-- | What reading a module established about its operators.
--
-- Declaring nothing is something a module did; being unreadable is
-- something that happened to us. Everything here turns on keeping those
-- apart, which is why this is two constructors rather than a map that
-- might be empty.
data Established
  = -- | It was read, and declares these.
    Declares (Map OpName Fixity)
  | -- | It could not be read. The expensive answer of the two, because
    -- reaching it means exhausting every way of reading the module.
    Unreadable
  deriving (Eq, Show)
