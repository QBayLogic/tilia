{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Working out the fixity of the operators a module uses.
module Linden.Fixity
  ( -- * Fixities
    OpName (..),
    Direction (..),
    Fixity (..),
    defaultFixity,

    -- * Layer 1: what a module declares
    declaredFixities,
    moduleName,

    -- * What a module passes on
    ExportItem (..),
    moduleExports,

    -- * Layer 2: what a module can see
    Import (..),
    moduleImports,
    Scope (..),
    resolveScope,

    -- * Answers
    Provenance (..),
    Resolution (..),
    lookupFixity,
    resolvedFixity,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs hiding (Fixity, OpName)
import GHC.Types.Fixity qualified as GHC
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
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
-- Layer 1: what a module declares

-- | The fixities a module declares for its own operators.
declaredFixities :: HsModule GhcPs -> Map OpName Fixity
declaredFixities =
  Map.fromList . concatMap fixitySig . hsmodDecls
  where
    fixitySig = \case
      L _ (SigD _ (FixSig _ (FixitySig _ names fixity))) ->
        [(opName (unLoc n), fromGhcFixity fixity) | n <- names]
      _ -> []

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
  = -- | A name, which may or may not be declared in this module.
    ExportName OpName
  | -- | @module M@, re-exporting everything that module brought in.
    ExportModule Text
  deriving (Eq, Show)

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
    named = ExportName . opName . ieWrappedName . unLoc

----------------------------------------------------------------------------
-- Layer 2: what a module can see

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
moduleImports :: HsModule GhcPs -> [Import]
moduleImports = map (fromDecl . unLoc) . hsmodImports
  where
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
    -- | Reachable as @M.op@, keyed by the alias actually written.
    scopeQualified :: Map (Text, OpName) (Fixity, Provenance),
    -- | Imported modules whose declarations could not be read.
    --
    -- While this is non-empty nothing can be said with certainty about an
    -- operator that was not found: the answer might be in here. It is what
    -- separates \"no declaration exists\" from \"we did not manage to look\".
    scopeUnreadable :: [Text],
    -- | Operators brought into unqualified scope with two different
    -- fixities. See the note in the module header: this should be empty for
    -- anything that compiles.
    scopeAmbiguous :: [OpName]
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
-- Not yet handled: re-export chains, and operators arriving through
-- @T(..)@. Both are syntactic and belong in the lookup function.
resolveScope ::
  -- | What a module exports, or 'Nothing' if that could not be determined
  (Text -> Maybe (Map OpName Fixity)) ->
  HsModule GhcPs ->
  Scope
resolveScope exportsOf hsModule =
  Scope
    { scopeUnqualified = Map.union own (Map.map fst unqualified),
      scopeQualified = qualified,
      scopeUnreadable = unreadable,
      scopeAmbiguous = Map.keys (Map.filter snd unqualified)
    }
  where
    own = Map.map (,DeclaredHere) (declaredFixities hsModule)
    imports = moduleImports hsModule

    unreadable =
      [importModule i | i <- imports, Nothing <- [exportsOf (importModule i)]]

    -- Paired with a flag saying whether two imports disagreed about it.
    unqualified =
      Map.unionsWith disagree
        [ Map.map (,False) (visible i)
        | i <- imports,
          not (importQualified i)
        ]
    disagree (a, aBad) (b, bBad) = (a, aBad || bBad || fst a /= fst b)

    qualified =
      Map.fromList
        [ ((importAlias i, op), entry)
        | i <- imports,
          (op, entry) <- Map.toList (visible i)
        ]

    -- What one import actually brings in, after its list is applied. A
    -- module we could not read brings in nothing, and is recorded in
    -- 'scopeUnreadable' so that its absence is not mistaken for emptiness.
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
    Unresolved [Text]
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
    Nothing -> case scopeUnreadable scope of
      -- Every module in scope was read and none declares it, so the
      -- Report's default is not a guess but a conclusion.
      [] -> Resolved defaultFixity ReportDefault
      missing -> Unresolved missing
  where
    found = case qualifier of
      Nothing -> Map.lookup op (scopeUnqualified scope)
      Just q -> Map.lookup (q, op) (scopeQualified scope)

-- | The fixity of a resolution, if it has one.
resolvedFixity :: Resolution -> Maybe Fixity
resolvedFixity = \case
  Resolved f _ -> Just f
  Unresolved _ -> Nothing
