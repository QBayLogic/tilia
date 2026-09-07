{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | An account of how a module's fixities were determined.
module Tilia.Fixity.Debug
  ( FixityNotes (..),
    ImportNote (..),
    OperatorNote (..),
    fixityNotes,
    renderFixityNotes,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs (HsModule)
import GHC.Hs.Extension (GhcPs)
import Tilia.Fixity
  ( Fixity (..),
    Direction (..),
    Import (..),
    OpName (..),
    Provenance (..),
    Resolution (..),
    Scope (..),
    lookupFixity,
    moduleImports,
    operatorSpelling,
    operatorsUsed,
  )
import Tilia.Palette (Color (Operator, Place), Palette, paint)
import Tilia.Utils (indent)

-- | Everything that decided one module's fixities.
data FixityNotes = FixityNotes
  { -- | What each import brought, in the order the module writes them.
    notedImports :: [ImportNote],
    -- | What became of every operator the module uses, one entry per
    -- operator.
    notedOperators :: [OperatorNote],
    -- | What the module declares for itself.
    notedDeclarations :: [(Text, Fixity)]
  }
  deriving (Eq, Show)

-- | One import.
data ImportNote = ImportNote
  { -- | The module imported.
    noteModule :: Text,
    -- | The name it goes under here, when that differs from its own.
    noteAlias :: Maybe Text,
    -- | Whether it was imported qualified.
    noteQualified :: Bool,
    -- | How many operators it was read for, or 'Nothing' when it could not
    -- be read at all.
    noteBrought :: Maybe Int
  }
  deriving (Eq, Show)

-- | One operator the module uses.
data OperatorNote = OperatorNote
  { -- | The operator as the module writes it, qualifier and all.
    noteSpelling :: Text,
    -- | What the scope answered for it.
    noteResolution :: Resolution,
    -- | Whether two modules in scope disagree about it.
    noteAmbiguous :: Bool
  }
  deriving (Eq, Show)

-- | Record everything that decided one module's fixities.
fixityNotes ::
  -- | What each module in scope exports, as the resolver answers it
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  -- | The scope the module was formatted under
  Scope ->
  -- | The module
  HsModule GhcPs ->
  IO FixityNotes
fixityNotes resolve scope hsModule = do
  brought <- traverse alongside (moduleImports hsModule)
  pure
    FixityNotes
      { notedImports = brought,
        notedOperators = map aboutOperator used,
        notedDeclarations = here
      }
  where
    alongside i = do
      answer <- resolve (importModule i)
      pure
        ImportNote
          { noteModule = importModule i,
            noteAlias =
              if importAlias i == importModule i
                then Nothing
                else Just (importAlias i),
            noteQualified = importQualified i,
            noteBrought = Map.size <$> answer
          }

    used =
      Map.elems (Map.fromList [(uncurry operatorSpelling u, u) | u <- operatorsUsed hsModule])

    here =
      [ (op, fixity)
      | (OpName op, (fixity, DeclaredHere)) <- Map.toList (scopeUnqualified scope)
      ]

    aboutOperator (qualifier, op) =
      OperatorNote
        { noteSpelling = operatorSpelling qualifier op,
          noteResolution = lookupFixity scope qualifier op,
          noteAmbiguous = (qualifier, op) `elem` scopeAmbiguous scope
        }

-- | Set out all the 'FixityNotes' per file.
renderFixityNotes :: Palette -> Map FilePath FixityNotes -> [Text]
renderFixityNotes palette notes =
  concat
    [ (indent 1 <> "fixities for " <> paint palette Place (T.pack path))
        : aboutFile palette told
    | (path, told) <- Map.toList notes
    ]

-- | One file's account, in reading order.
aboutFile :: Palette -> FixityNotes -> [Text]
aboutFile palette notes =
  concat
    [ [heading "imports"],
      map (entry . fromImport) (notedImports notes),
      [heading "operators"],
      map (entry . fromOperator) (notedOperators notes),
      [heading "declared here" | not (null (notedDeclarations notes))],
      map (entry . fromOwn) (notedDeclarations notes)
    ]
  where
    heading what = indent 2 <> "· " <> what
    entry line = indent 3 <> "· " <> line

    fromImport i =
      named (noteModule i)
        <> qualification i
        <> ": "
        <> case noteBrought i of
          Nothing -> "could not be read"
          Just n -> operators n

    qualification i = case (noteQualified i, noteAlias i) of
      (True, Just alias) -> " qualified as " <> named alias
      (True, Nothing) -> " qualified"
      (False, Just alias) -> " as " <> named alias
      (False, Nothing) -> ""

    fromOwn (op, fixity) = operator op <> " " <> spelled fixity

    fromOperator o =
      operator (noteSpelling o)
        <> " "
        <> case noteResolution o of
          Resolved fixity provenance ->
            spelled fixity <> ", " <> from provenance <> ambiguously o
          Unresolved missing ->
            "unknown: may be declared in "
              <> T.intercalate " or " (map named (NE.toList missing))
              <> ", which could not be read"

    from = \case
      DeclaredHere -> "declared in this module"
      DeclaredIn m -> "declared in " <> named m
      ReportDefault -> "the Report's default, nothing in scope declaring it"

    ambiguously o
      | noteAmbiguous o = ", and two modules in scope disagree about it"
      | otherwise = ""

    named = paint palette Place
    operator = paint palette Operator

    operators = \case
      1 -> "1 operator"
      n -> T.pack (show n) <> " operators"

-- | A fixity, written the way it would be declared.
spelled :: Fixity -> Text
spelled (Fixity direction precedence) =
  which direction <> " " <> T.pack (show precedence)
  where
    which = \case
      LeftAssoc -> "infixl"
      RightAssoc -> "infixr"
      NoAssoc -> "infix"
