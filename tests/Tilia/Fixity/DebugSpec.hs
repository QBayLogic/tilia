{-# LANGUAGE OverloadedStrings #-}

-- | The account a run gives of how it settled a module's operators.
module Tilia.Fixity.DebugSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Tilia.Fixity
  ( Direction (..),
    Fixity (..),
    Known (..),
    OpName (..),
    nothingKnown,
    resolveScope,
  )
import Tilia.Fixity.Debug (fixityNotes, renderFixityNotes)
import Tilia.Palette (Palette (Plain))
import Tilia.Parser (defaultParserConfig, describeParseError, parseModule, pmModule)

spec :: Spec
spec = do
  describe "what each import brought" $ do
    it "counts the operators a module was read for" $
      notesFor [("Prelude", Just []), ("Data.Map", Just [("!", infixl' 9)])] "import Data.Map\n"
        >>= (`shouldContain'` "· Data.Map: 1 operator")

    it "says so when a module could not be read" $
      notesFor [("Prelude", Just [])] "import Criterion.Main\n"
        >>= (`shouldContain'` "· Criterion.Main: could not be read")

    it "keeps the alias a qualified import goes under" $
      notesFor [("Prelude", Just []), ("Data.Map", Just [])] "import qualified Data.Map as M\n"
        >>= (`shouldContain'` "· Data.Map qualified as M: 0 operators")

    it "names the Prelude, which nobody wrote but everybody imports" $
      notesFor [("Prelude", Just [("+", infixl' 6)])] "f = 1\n"
        >>= (`shouldContain'` "· Prelude: 1 operator")

  describe "what became of each operator" $ do
    it "names the import that carried the fixity" $
      notesFor
        [("Prelude", Just []), ("Data.Map", Just [("!", infixl' 9)])]
        "import Data.Map\nf m = m ! 1\n"
        >>= (`shouldContain'` "· ! infixl 9, declared in Data.Map")

    it "says when the module declared it itself" $
      notesFor [("Prelude", Just [])] "infixr 5 <+>\nf a b = a <+> b\n"
        >>= (`shouldContain'` "· <+> infixr 5, declared in this module")

    it "says when nothing in scope declares it and everything was read" $
      notesFor [("Prelude", Just [])] "f a b = a <?> b\n"
        >>= (`mentions` "<?> infixl 9, the Report's default")

    it "says which unread module the answer might have been in" $
      notesFor [("Prelude", Just [])] "import Criterion.Main\nf a b = a <?> b\n"
        >>= ( `shouldContain'`
                "· <?> unknown: may be declared in Criterion.Main, which this run could not read"
            )

    it "counts the unread modules when there is more than one" $
      notesFor
        [("Prelude", Just [])]
        "import Criterion.Main\nimport Test.Tasty\nf a b = a <?> b\n"
        >>= ( `mentions`
                "may be declared in Criterion.Main or Test.Tasty, neither of which this run could read"
            )

    it "keeps the qualifier an operator was written under" $
      notesFor
        [("Prelude", Just []), ("Data.Map", Just [("!", infixl' 9)])]
        "import qualified Data.Map as M\nf m = m M.! 1\n"
        >>= (`shouldContain'` "· M.! infixl 9, declared in Data.Map")

    it "says when two imports disagree about one" $
      notesFor
        [ ("Prelude", Just []),
          ("Left", Just [("<+>", infixl' 6)]),
          ("Right", Just [("<+>", Fixity RightAssoc 5)])
        ]
        "import Left\nimport Right\nf a b = a <+> b\n"
        >>= (`mentions` "two modules in scope disagree about it")

    it "gives an operator one line however often it is written" $ do
      told <- notesFor [("Prelude", Just [])] "f a b c = a <?> b <?> c <?> a\n"
      length (filter (T.isInfixOf "<?>") told) `shouldBe` 1

  describe "the shape of it" $ do
    it "sets out under headings" $ do
      told <- notesFor [("Prelude", Just [])] "f = 1\n"
      map T.stripStart told `shouldContain` ["· imports"]
      map T.stripStart told `shouldContain` ["· operators"]

    it "indents an entry further than the heading it sits under" $ do
      told <- notesFor [("Prelude", Just [])] "import Data.Map\n"
      let indentOf = T.length . T.takeWhile (== ' ')
          under heading = [indentOf l | l <- told, heading `T.isInfixOf` l]
      case (under "· imports", under "· Data.Map") of
        ([heading], [there]) -> there `shouldSatisfy` (> heading)
        (headings, entries) ->
          expectationFailure (show (headings, entries))

    it "says nothing about declarations a module does not make" $
      notesFor [("Prelude", Just [])] "f = 1\n"
        >>= (`shouldSatisfy` all (not . T.isInfixOf "declared here"))

    it "lists what the module declares for itself" $
      notesFor [("Prelude", Just [])] "infixr 5 <+>\nf a b = a <+> b\n"
        >>= (`shouldContain'` "· <+> infixr 5")

----------------------------------------------------------------------------
-- Helpers

-- | The account given of a module, against a world of imports that could be
-- read and imports that could not.
--
-- A module named in the world is readable and exports what is listed; a
-- module absent from it is one the resolver could not read at all.
notesFor :: [(Text, Maybe [(Text, Fixity)])] -> Text -> IO [Text]
notesFor world source =
  renderFixityNotes Plain . Map.singleton "M.hs"
    <$> fixityNotes (pure . exportsOf) scope hsModule
  where
    scope = resolveScope nothingKnown {knownFixities = exportsOf} hsModule
    hsModule = pmModule parsed
    parsed = case parseModule defaultParserConfig "M.hs" ("module M where\n" <> source) of
      Left problem -> error (T.unpack (describeParseError problem))
      Right m -> m
    exportsOf m = do
      declared <- lookup m world
      Map.fromList . map (\(op, fixity) -> (OpName op, fixity)) <$> declared

infixl' :: Int -> Fixity
infixl' = Fixity LeftAssoc

-- | Is this line among them, whatever it was indented by?
shouldContain' :: [Text] -> Text -> Expectation
shouldContain' told wanted =
  map T.stripStart told `shouldContain` [wanted]

-- | Does some line say this much, whatever else it goes on to say?
mentions :: [Text] -> Text -> Expectation
mentions told wanted = told `shouldSatisfy` any (T.isInfixOf wanted)
