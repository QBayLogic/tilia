{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Whether fixities can be resolved exactly from source alone.
module Tilia.FixitySpec (spec) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Test.Hspec
import Tilia.Fixity
import Tilia.Parser

spec :: Spec
spec = do
  describe "layer 1: what a module declares" $ do
    it "reads a left-associative declaration" $
      declaredIn "module M where\ninfixl 6 <+>\n"
        `shouldBe` [(OpName "<+>", Fixity LeftAssoc 6)]

    it "reads a right-associative declaration" $
      declaredIn "module M where\ninfixr 5 <>>\n"
        `shouldBe` [(OpName "<>>", Fixity RightAssoc 5)]

    it "reads a non-associative declaration" $
      declaredIn "module M where\ninfix 4 ===\n"
        `shouldBe` [(OpName "===", Fixity NoAssoc 4)]

    it "reads several operators from one declaration" $
      declaredIn "module M where\ninfixl 7 <.>, <:>\n"
        `shouldBe` [(OpName "<.>", Fixity LeftAssoc 7), (OpName "<:>", Fixity LeftAssoc 7)]

    it "reads a backticked function name" $
      declaredIn "module M where\ninfixl 7 `quot`\n"
        `shouldBe` [(OpName "quot", Fixity LeftAssoc 7)]

    it "finds nothing when nothing is declared" $
      declaredIn "module M where\nx = 1\n" `shouldBe` []

    it "reads a declaration that appears after its use" $
      declaredIn "module M where\ny = a <+> b\ninfixl 6 <+>\n"
        `shouldBe` [(OpName "<+>", Fixity LeftAssoc 6)]

    it "reads one a class makes about its own method" $
      declaredIn "module M where\nclass C a where\n  infixr 8 .=\n  (.=) :: a -> a -> Int\n"
        `shouldBe` [(OpName ".=", Fixity RightAssoc 8)]

    it "reads those at the margin and in a class together" $
      declaredIn "module M where\ninfixl 1 <+>\nclass C a where\n  infixr 8 .=\n  (.=) :: a -> a -> Int\n"
        `shouldBe` [(OpName ".=", Fixity RightAssoc 8), (OpName "<+>", Fixity LeftAssoc 1)]

    it "leaves a declaration local to a binding where it is" $
      declaredIn "module M where\nf = g\n  where\n    infixr 3 ###\n    g = 1\n"
        `shouldBe` []

  describe "what a module says it exports" $ do
    it "has nothing to say about a module with no export list" $
      exportsOfSource "module M where\nf = 1\n" `shouldBe` Nothing

    it "keeps the qualifier a name was written under" $
      exportsOfSource "module M ((Disp.<+>)) where\n"
        `shouldBe` Just [ExportName (Just "Disp") (OpName "<+>")]

    it "has none for a name written plainly" $
      exportsOfSource "module M ((<+>)) where\n"
        `shouldBe` Just [ExportName Nothing (OpName "<+>")]

    it "reads a whole module passed on as the module it names" $
      exportsOfSource "module M (module Data.Map) where\n"
        `shouldBe` Just [ExportModule "Data.Map"]

  describe "the operators an export list names" $ do
    it "takes them from an explicit list" $
      exportedIn "module M ((<+>), (<?>), f) where\n"
        `shouldBe` Just [OpName "<+>", OpName "<?>", OpName "f"]

    it "takes the members of a class the module declares itself" $
      exportedIn
        "module M (C (..)) where\nclass C a where\n  infixr 8 .=\n  (.=) :: a -> a -> Int\n"
        `shouldBe` Just [OpName ".=", OpName "C"]

    it "takes the constructors of a type the module declares itself" $
      exportedIn "module M (T (..)) where\ndata T = A | Int :| Int\n"
        `shouldBe` Just [OpName ":|", OpName "A", OpName "T"]

    it "knows nothing of a type the module only passes on" $
      exportedIn "module M (C (..)) where\nimport Elsewhere\n" `shouldBe` Nothing

    it "knows nothing of a module that hands a whole module on" $
      exportedIn "module M ((<+>), module Data.Map) where\n" `shouldBe` Nothing

    it "takes what a module with no export list declares" $
      exportedIn "module M where\ninfixr 5 <+>\ninfixl 6 <?>\n"
        `shouldBe` Just [OpName "<+>", OpName "<?>"]

    it "finds nothing in a module with no list and no declarations" $
      exportedIn "module M where\nf = 1\n" `shouldBe` Just []

  describe "which unread module an unsettled operator is blamed on" $ do
    it "passes over one whose export list has no such operator" $
      lookupFixity (scopeKnowing [("Opaque", ["<+>"])] usesUnknown) Nothing (OpName "<??>")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "blames one whose export list names it" $
      lookupFixity (scopeKnowing [("Opaque", ["<??>"])] usesUnknown) Nothing (OpName "<??>")
        `shouldBe` Unresolved ("Opaque" :| [])

    it "blames one that will not say what it exports" $
      lookupFixity (scopeKnowing [] usesUnknown) Nothing (OpName "<??>")
        `shouldBe` Unresolved ("Opaque" :| [])

    it "still passes over an import list that does not name it" $
      lookupFixity
        (scopeKnowing [("Opaque", ["<??>"])] "module M where\nimport Opaque ((<+>))\n")
        Nothing
        (OpName "<??>")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "blames only the ones that could supply it, of several unread" $
      lookupFixity
        (scopeKnowing [("Opaque", ["<+>"]), ("Other.Opaque", ["<??>"])] twoUnread)
        Nothing
        (OpName "<??>")
        `shouldBe` Unresolved ("Other.Opaque" :| [])

    it "settles nothing on its own account when told nothing" $
      lookupFixity (fullScope usesUnknown) Nothing (OpName "<??>")
        `shouldBe` Unresolved ("Opaque" :| [])

    it "passes over one whose import list carries no such operator" $
      lookupFixity
        (scopeSuspecting [("Opaque", [("T", ["<+>"])])] "import Opaque (T (..))\n")
        Nothing
        (OpName "<??>")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "blames one whose import list carries it" $
      lookupFixity
        (scopeSuspecting [("Opaque", [("T", ["<??>"])])] "import Opaque (T (..))\n")
        Nothing
        (OpName "<??>")
        `shouldBe` Unresolved ("Opaque" :| [])

    it "blames one whose (..) nothing is known about" $
      lookupFixity
        (scopeSuspecting [] "import Opaque (T (..))\n")
        Nothing
        (OpName "<??>")
        `shouldBe` Unresolved ("Opaque" :| [])

    it "passes over one that hides the operator along with its type" $
      lookupFixity
        (scopeSuspecting [("Opaque", [("T", ["<??>"])])] "import Opaque hiding (T (..))\n")
        Nothing
        (OpName "<??>")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "lets a file be formatted when no unread module could have declared it" $
      unknownOperators
        (scopeKnowing [("Opaque", ["<+>"])] usesUnknown)
        (pmModule (parsed usesUnknown))
        `shouldBe` []

  describe "what a name carries with it" $ do
    it "takes a type's constructors" $
      childrenIn "module M where\ndata T = A | Int :| Int\n"
        `shouldBe` [(OpName "T", [OpName ":|", OpName "A"])]

    it "takes a record's fields, which may be operators" $
      childrenIn "module M where\ndata T = T {(#) :: Int, name :: Int}\n"
        `shouldBe` [(OpName "T", [OpName "#", OpName "T", OpName "name"])]

    it "takes a GADT's constructors" $
      childrenIn "module M where\ndata T where\n  A :: T\n  (:|) :: T -> T\n"
        `shouldBe` [(OpName "T", [OpName ":|", OpName "A"])]

    it "takes a class's methods" $
      childrenIn "module M where\nclass C a where\n  (.=) :: a -> a -> Int\n  named :: a\n"
        `shouldBe` [(OpName "C", [OpName ".=", OpName "named"])]

    it "takes a class's associated families" $
      childrenIn "module M where\nclass C a where\n  type F a\n"
        `shouldBe` [(OpName "C", [OpName "F"])]

    it "has nothing to say about a type synonym" $
      childrenIn "module M where\ntype T = Int\n" `shouldBe` []

    it "keeps to what an export list hands on" $
      exportedChildrenIn "module M (T (A)) where\ndata T = A | Int :| Int\n"
        `shouldBe` [(OpName "T", [OpName "A"])]

    it "hands on everything under a name exported with (..)" $
      exportedChildrenIn "module M (T (..)) where\ndata T = A | Int :| Int\n"
        `shouldBe` [(OpName "T", [OpName ":|", OpName "A"])]

    it "hands on nothing under a type it does not declare" $
      exportedChildrenIn "module M (T (..)) where\nimport Elsewhere\n"
        `shouldBe` [(OpName "T", [])]

  describe "what an import list brings in" $ do
    it "brings in what a name carries, where that is known" $
      brought [(OpName "NonEmpty", [OpName ":|"])] "import Data.List.NonEmpty (NonEmpty (..))\n"
        `shouldBe` [OpName ":|", OpName "NonEmpty"]

    it "brings in the members written out beside a name" $
      brought [] "import Data.List.NonEmpty (NonEmpty ((:|)))\n"
        `shouldBe` [OpName ":|", OpName "NonEmpty"]

    it "brings in a plain name and nothing else" $
      brought [(OpName "NonEmpty", [OpName ":|"])] "import Data.List.NonEmpty (toList)\n"
        `shouldBe` [OpName "toList"]

    it "brings in only the name itself where nothing is known" $
      brought [] "import Data.List.NonEmpty (NonEmpty (..))\n"
        `shouldBe` [OpName "NonEmpty"]

  describe "layer 2: imports" $ do
    it "brings in an operator a type carries" $
      lookupFixity (scopeCarrying "import Carrier (T (..))\n") Nothing (OpName ":|")
        `shouldBe` Resolved (Fixity RightAssoc 5) (DeclaredIn "Carrier")

    it "leaves out an operator the type does not carry" $
      lookupFixity (scopeCarrying "import Carrier (T (..))\n") Nothing (OpName "<+>")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "hides an operator hidden along with its type" $
      lookupFixity (scopeCarrying "import Carrier hiding (T (..))\n") Nothing (OpName ":|")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "keeps what a hiding list leaves alone" $
      lookupFixity (scopeCarrying "import Carrier hiding (T (..))\n") Nothing (OpName "<+>")
        `shouldBe` Resolved (Fixity LeftAssoc 6) (DeclaredIn "Carrier")

    it "brings it in under a qualifier too" $
      lookupFixity
        (scopeCarrying "import qualified Carrier as C (T (..))\n")
        (Just "C")
        (OpName ":|")
        `shouldBe` Resolved (Fixity RightAssoc 5) (DeclaredIn "Carrier")

    it "sees an unqualified import in both scopes" $
      scopeOf "module M where\nimport Data.Map\n"
        `shouldBe` ( [(OpName "!", Fixity LeftAssoc 9)],
                     [(("Data.Map", OpName "!"), Fixity LeftAssoc 9)],
                     []
                   )

    it "does not bring a qualified import into unqualified scope" $
      scopeOf "module M where\nimport qualified Data.Map\n"
        `shouldBe` ([], [(("Data.Map", OpName "!"), Fixity LeftAssoc 9)], [])

    it "makes an alias the qualifier" $
      scopeOf "module M where\nimport qualified Data.Map as M\n"
        `shouldBe` ([], [(("M", OpName "!"), Fixity LeftAssoc 9)], [])

    it "keeps unqualified names when an alias is not qualified" $
      scopeOf "module M where\nimport Data.Map as M\n"
        `shouldBe` ( [(OpName "!", Fixity LeftAssoc 9)],
                     [(("M", OpName "!"), Fixity LeftAssoc 9)],
                     []
                   )

    it "honours an explicit import list" $
      scopeOf "module M where\nimport Data.Sequence ((|>))\n"
        `shouldBe` ( [(OpName "|>", Fixity LeftAssoc 5)],
                     [(("Data.Sequence", OpName "|>"), Fixity LeftAssoc 5)],
                     []
                   )

    it "honours a hiding list" $
      scopeOf "module M where\nimport Data.Sequence hiding ((|>))\n"
        `shouldBe` ( [(OpName "<|", Fixity RightAssoc 5)],
                     [(("Data.Sequence", OpName "<|"), Fixity RightAssoc 5)],
                     []
                   )

    it "lets the module's own declaration win over an import" $
      let (unq, _, _) = scopeOf "module M where\nimport Data.Map\ninfixr 3 !\n"
       in unq `shouldBe` [(OpName "!", Fixity RightAssoc 3)]

  describe "ambiguity" $ do
    it "reports an operator imported with two different fixities" $
      let (_, _, amb) = scopeOf "module M where\nimport Data.Map\nimport Other\n"
       in amb `shouldBe` [(Nothing, OpName "!")]

    it "reports nothing when two imports agree" $
      let (_, _, amb) = scopeOf "module M where\nimport Data.Map\nimport Agreeing\n"
       in amb `shouldBe` []

    it "reports nothing when the two go under different names" $
      let (_, _, amb) =
            scopeOf "module M where\nimport Data.Map\nimport qualified Other\n"
       in amb `shouldBe` []

    it "reports an alias two imports disagree under" $
      let (_, _, amb) =
            scopeOf
              "module M where\nimport qualified Data.Map as M\nimport qualified Other as M\n"
       in amb `shouldBe` [(Just "M", OpName "!")]

    it "reports nothing when two imports under one alias agree" $
      let (_, _, amb) =
            scopeOf
              "module M where\nimport qualified Data.Map as M\nimport qualified Agreeing as M\n"
       in amb `shouldBe` []

    it "counts an unqualified import towards the name it goes under" $
      let (_, _, amb) =
            scopeOf
              "module M where\nimport Data.Map\nimport qualified Other as Data.Map\n"
       in amb `shouldBe` [(Just "Data.Map", OpName "!")]

    it "keeps a clashing alias apart from a clashing bare name" $
      let (_, _, amb) =
            scopeOf
              "module M where\nimport Data.Map\nimport Other\nimport qualified Data.Map as M\nimport qualified Other as M\n"
       in amb `shouldBe` [(Nothing, OpName "!"), (Just "M", OpName "!")]

  describe "lookupFixity" $ do
    it "finds an unqualified operator" $
      let s = fullScope "module M where\nimport Data.Map\n"
       in lookupFixity s Nothing (OpName "!")
            `shouldBe` Resolved (Fixity LeftAssoc 9) (DeclaredIn "Data.Map")

    it "finds a qualified operator through its alias" $
      let s = fullScope "module M where\nimport qualified Data.Map as M\n"
       in lookupFixity s (Just "M") (OpName "!")
            `shouldBe` Resolved (Fixity LeftAssoc 9) (DeclaredIn "Data.Map")

    it "concludes infixl 9 when every module in scope was read" $
      let s = fullScope "module M where\nimport Data.Map\n"
       in lookupFixity s Nothing (OpName "<??>")
            `shouldBe` Resolved defaultFixity ReportDefault

    it "refuses to conclude anything when a module could not be read" $
      let s = fullScope "module M where\nimport Data.Map\nimport Opaque\n"
       in lookupFixity s Nothing (OpName "<??>")
            `shouldBe` Unresolved ("Opaque" :| [])

    it "still answers for an operator it did find, despite an unread module" $
      let s = fullScope "module M where\nimport Data.Map\nimport Opaque\n"
       in lookupFixity s Nothing (OpName "!")
            `shouldBe` Resolved (Fixity LeftAssoc 9) (DeclaredIn "Data.Map")

    it "attributes the module\'s own declaration to itself" $
      let s = fullScope "module M where\ninfixr 3 <+>\n"
       in lookupFixity s Nothing (OpName "<+>")
            `shouldBe` Resolved (Fixity RightAssoc 3) DeclaredHere

    it "does not find a qualified-only operator unqualified" $
      let s = fullScope "module M where\nimport qualified Data.Map\n"
       in lookupFixity s Nothing (OpName "!")
            `shouldBe` Resolved defaultFixity ReportDefault

  describe "a qualified use is answered from qualified scope alone" $ do
    it "does not answer a qualifier that brought nothing in from what did" $
      let s = fullScope "module M where\nimport Data.Map\n"
       in lookupFixity s (Just "Q") (OpName "!")
            `shouldBe` Resolved defaultFixity ReportDefault

    it "does not lend the module's own declaration to a foreign qualifier" $
      let s = fullScope "module M where\ninfixr 3 <+>\n"
       in lookupFixity s (Just "Q") (OpName "<+>")
            `shouldBe` Resolved defaultFixity ReportDefault

    it "does not answer through an alias the import does not go under" $
      let s = fullScope "module M where\nimport qualified Data.Map as M\n"
       in lookupFixity s (Just "Data.Map") (OpName "!")
            `shouldBe` Resolved defaultFixity ReportDefault

    it "answers a use qualified by the module's own name" $
      let s = fullScope "module M where\ninfixr 3 <+>\n"
       in lookupFixity s (Just "M") (OpName "<+>")
            `shouldBe` Resolved (Fixity RightAssoc 3) DeclaredHere

    it "answers a plain import under the module's own name" $
      let s = fullScope "module M where\nimport Data.Map\n"
       in lookupFixity s (Just "Data.Map") (OpName "!")
            `shouldBe` Resolved (Fixity LeftAssoc 9) (DeclaredIn "Data.Map")

    it "weighs only the unread imports the qualifier reaches" $
      let s = fullScope "module M where\nimport qualified Data.Map as M\nimport Opaque\n"
       in lookupFixity s (Just "M") (OpName "<??>")
            `shouldBe` Resolved defaultFixity ReportDefault

    it "refuses to conclude when the qualifier reaches an unread import" $
      let s = fullScope "module M where\nimport qualified Opaque as O\n"
       in lookupFixity s (Just "O") (OpName "<??>")
            `shouldBe` Unresolved ("Opaque" :| [])

  describe "what could not be settled" $ do
    it "says which qualifier the unsettled use was written under" $
      unsettledIn "module M where\nimport qualified Opaque as O\nf a b = a O.<+> b\n"
        `shouldBe` [((Just "O", OpName "<+>"), NotRead ("Opaque" :| []))]

    it "keeps a qualified use apart from an unqualified one" $
      unsettledIn
        "module M where\nimport Opaque\nimport qualified Opaque as O\nf a b = a <+> b\ng a b = a O.<+> b\n"
        `shouldBe` [ ((Nothing, OpName "<+>"), NotRead ("Opaque" :| [])),
                     ((Just "O", OpName "<+>"), NotRead ("Opaque" :| []))
                   ]

    it "leaves a settled qualified use out, unread imports notwithstanding" $
      unsettledIn "module M where\nimport qualified Data.Map as M\nimport Opaque\nf m = m M.! 1\n"
        `shouldBe` []

    it "holds an ambiguous operator against its unqualified use only" $
      unsettledIn
        "module M where\nimport Data.Map\nimport Other\nimport qualified Data.Map as M\nf a b = (a ! b, a M.! b)\n"
        `shouldBe` [((Nothing, OpName "!"), Ambiguous)]

    it "holds a clashing alias against the use written under it" $
      unsettledIn
        "module M where\nimport qualified Data.Map as M\nimport qualified Other as M\nf a b = a M.! b\n"
        `shouldBe` [((Just "M", OpName "!"), Ambiguous)]

    it "leaves the bare operator alone when only an alias is in doubt" $
      unsettledIn
        "module M where\nimport Data.Map\nimport qualified Data.Map as M\nimport qualified Other as M\nf a b = (a ! b, a M.! b)\n"
        `shouldBe` [((Just "M", OpName "!"), Ambiguous)]

    it "spells a use the way the module wrote it" $
      map (uncurry operatorSpelling . fst) (unsettledIn "module M where\nimport qualified Opaque as O\nf a b = a O.<+> b\n")
        `shouldBe` ["O.<+>"]

  describe "parsing with the module's own pragmas"
    $ it "parses a module that needs an extension it declares"
    $ declaredIn "{-# LANGUAGE MagicHash #-}\nmodule M where\ninfixl 6 <+>\n"
      `shouldBe` [(OpName "<+>", Fixity LeftAssoc 6)]

----------------------------------------------------------------------------
-- Helpers

-- | Stand-in for layer 3. The real one reads a build plan, maps modules to
-- packages and parses their sources; what it returns is exactly this shape,
-- so everything above it can be exercised without any of that.
exportsOf :: Text -> Maybe (Map.Map OpName Fixity)
exportsOf = \case
  "Data.Map" -> Just (Map.fromList [(OpName "!", Fixity LeftAssoc 9)])
  "Data.Sequence" ->
    Just
      ( Map.fromList
          [ (OpName "|>", Fixity LeftAssoc 5),
            (OpName "<|", Fixity RightAssoc 5)
          ]
      )
  "Other" -> Just (Map.fromList [(OpName "!", Fixity RightAssoc 4)])
  "Agreeing" -> Just (Map.fromList [(OpName "!", Fixity LeftAssoc 9)])
  "Carrier" ->
    Just
      ( Map.fromList
          [ (OpName ":|", Fixity RightAssoc 5),
            (OpName "<+>", Fixity LeftAssoc 6)
          ]
      )
  "Opaque" -> Nothing
  "Other.Opaque" -> Nothing
  _ -> Just Map.empty

parsed :: Text -> ParsedModule
parsed src = case parseModule defaultParserConfig "test.hs" src of
  Left _ -> error "the test input did not parse"
  Right pm -> pm

declaredIn :: Text -> [(OpName, Fixity)]
declaredIn = Map.toList . declaredFixities . pmModule . parsed

exportsOfSource :: Text -> Maybe [ExportItem]
exportsOfSource = moduleExports . pmModule . parsed

-- | A scope knowing what every module it can read exports, and nothing
-- about the ones it cannot.
fullScope :: Text -> Scope
fullScope = resolveScope knowingExports . pmModule . parsed

-- | What is known in a world made of 'exportsOf' alone.
knowingExports :: Known
knowingExports = nothingKnown {knownFixities = exportsOf}

-- | A module that uses an operator nothing in scope declares, alongside an
-- import that could not be read.
usesUnknown :: Text
usesUnknown = "module M where\nimport Opaque\nf a b = a <??> b\n"

-- | The same, with a second unread import to tell apart from the first.
twoUnread :: Text
twoUnread = "module M where\nimport Opaque\nimport Other.Opaque\nf a b = a <??> b\n"

-- | A scope in which the unread modules listed say what they export.
--
-- A module absent from the list says nothing, which is what 'fullScope'
-- assumes of every one of them.
scopeKnowing :: [(Text, [Text])] -> Text -> Scope
scopeKnowing said =
  resolveScope knowingExports {knownExportNames = exportNamesOf} . pmModule . parsed
  where
    exportNamesOf m = Set.fromList . map OpName <$> lookup m said

-- | What each name a module declares carries with it, in a settled order.
childrenIn :: Text -> [(OpName, [OpName])]
childrenIn = settled . declaredChildren . pmModule . parsed

-- | The same, as the module's export list hands them on.
exportedChildrenIn :: Text -> [(OpName, [OpName])]
exportedChildrenIn = settled . moduleChildren . pmModule . parsed

settled :: Map.Map OpName (Set.Set OpName) -> [(OpName, [OpName])]
settled = map (fmap Set.toList) . Map.toList

-- | The names one import list brings in, told what the module it names
-- keeps under each of its names.
brought :: [(OpName, [OpName])] -> Text -> [OpName]
brought carries source =
  case [items | i <- written, Just (_, items) <- [importNames i]] of
    [items] -> Set.toList (namesImported children items)
    other -> error ("expected one import with a list, got " <> show other)
  where
    written = moduleImports (pmModule (parsed ("module M where\n" <> source)))
    children = Map.fromList [(parent, Set.fromList kids) | (parent, kids) <- carries]

-- | A scope over a world where Carrier keeps @:|@ under @T@.
--
-- Carrier declares @<+>@ as well, under nothing, so that a list naming
-- @T(..)@ can be seen to bring the one in and leave the other out.
scopeCarrying :: Text -> Scope
scopeCarrying source =
  resolveScope
    knowingExports {knownChildren = childrenOf}
    (pmModule (parsed ("module M where\n" <> source)))
  where
    childrenOf = \case
      "Carrier" -> Map.fromList [(OpName "T", Set.fromList [OpName ":|"])]
      _ -> Map.empty

-- | A scope over an unread module, told what it keeps under its names.
--
-- Opaque cannot be read for fixities and says nothing about what it
-- exports, so what the import list brings in is all there is to go on.
scopeSuspecting :: [(Text, [(Text, [Text])])] -> Text -> Scope
scopeSuspecting carries source =
  resolveScope
    knowingExports {knownChildren = childrenOf}
    (pmModule (parsed ("module M where\n" <> source <> "f a b = a <??> b\n")))
  where
    childrenOf m =
      Map.fromList
        [ (OpName parent, Set.fromList (map OpName kids))
        | (parent, kids) <- Map.findWithDefault [] m (Map.fromList carries)
        ]

-- | The operators a module's export list names, in a settled order.
exportedIn :: Text -> Maybe [OpName]
exportedIn = fmap Set.toList . exportedOperators . pmModule . parsed

-- | The uses of an operator a module makes that its scope cannot settle.
unsettledIn :: Text -> [((Maybe Text, OpName), Unknown)]
unsettledIn src =
  let hsModule = pmModule (parsed src)
   in unknownOperators (resolveScope knowingExports hsModule) hsModule

scopeOf ::
  Text ->
  ( [(OpName, Fixity)],
    [((Text, OpName), Fixity)],
    [(Maybe Text, OpName)]
  )
scopeOf src =
  let s = fullScope src
   in ( Map.toList (Map.map fst (scopeUnqualified s)),
        Map.toList (Map.map fst (scopeQualified s)),
        scopeAmbiguous s
      )
