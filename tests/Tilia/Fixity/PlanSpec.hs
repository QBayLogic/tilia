{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The whole fixity pipeline, run against this project's own dependencies.
--
-- These tests read the real build plan, the real package cache and real
-- Hackage sources. That is the point: every other test in the suite works
-- on constructed inputs, and constructed inputs are exactly what a pipeline
-- that talks to the outside world will not fail on.
--
-- Running the test suite implies the project was built, so the plan and the
-- sources are there. Where they are not — a sandboxed build with no package
-- cache — each test says so and is marked pending rather than failing.
module Tilia.Fixity.PlanSpec (spec) where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeBaseName, takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Tilia.Fixity
import Tilia.Fixity.Plan
import Tilia.Parser

spec :: Spec
spec = do
  preparation
  reexports
  plan <- runIO (readBuildPlan (planPathFor "."))
  case plan of
    Left _ -> unavailable "no build plan; run cabal build first"
    Right p -> withPlan p

-- | What a run does before it trusts the plan.
--
-- These need no plan of their own and no @cabal@: the point is the order of
-- the steps, so the steps are recorded rather than taken.
preparation :: Spec
preparation = describe "preparing a project" $ do
  it "solves and then fetches, in the one run" $
    withTempProject Nothing $ \dir -> do
      steps <- newIORef []
      let cabal args = do
            record steps args
            when (args == ["build", "--dry-run"]) (writePlan dir wantingATarball)
            pure (Right ())
      checkReadiness dir `shouldReturn` PlanMissing
      prepareWith cabal dir PlanMissing `shouldReturn` Right ()
      readIORef steps
        `shouldReturn` [["build", "--dry-run"], ["build", "--only-download"]]

  it "fetches without solving when the plan is already good" $
    withTempProject (Just wantingATarball) $ \dir -> do
      steps <- newIORef []
      readiness <- checkReadiness dir
      readiness `shouldBe` SourcesMissing ["tilia-phantom"]
      prepareWith (obliging steps) dir readiness `shouldReturn` Right ()
      readIORef steps `shouldReturn` [["build", "--only-download"]]

  it "runs nothing at all when nothing is missing" $ do
    steps <- newIORef []
    prepareWith (obliging steps) "." Ready `shouldReturn` Right ()
    readIORef steps `shouldReturn` []

  it "does not go on to fetch when the solve fails" $
    withTempProject Nothing $ \dir -> do
      steps <- newIORef []
      let cabal args = record steps args >> pure (Left "cabal said no")
      prepareWith cabal dir PlanMissing `shouldReturn` Left "cabal said no"
      readIORef steps `shouldReturn` [["build", "--dry-run"]]

-- | Chasing an operator a module passes on rather than declares.
--
-- No plan and no network here: the modules a name could have come from
-- answer out of a table written below, which is what makes it possible to
-- ask not merely whether an answer came back but which module it came from.
reexports :: Spec
reexports = describe "an operator a module passes on" $ do
  it "comes from the module the qualifier names" $
    chased "module M ((Disp.<+>)) where\nimport Control.Arrow (first)\nimport qualified Text.PrettyPrint as Disp\n"
      `shouldReturn` Just (Fixity LeftAssoc 6)

  it "comes from an import that brings it in, not one that hides it" $
    chased "module M ((<+>)) where\nimport Control.Arrow hiding ((<+>))\nimport Text.PrettyPrint\n"
      `shouldReturn` Just (Fixity LeftAssoc 6)

  it "comes from an import that brings it in, not one that never names it" $
    chased "module M ((<+>)) where\nimport Control.Arrow (first)\nimport Text.PrettyPrint\n"
      `shouldReturn` Just (Fixity LeftAssoc 6)

  it "does not come from a qualified import when it is written plainly" $
    chased "module M ((<+>)) where\nimport Control.Arrow\nimport qualified Text.PrettyPrint as Disp\n"
      `shouldReturn` Just (Fixity RightAssoc 5)

  it "is the module's own where the module declares it" $
    chased "module M ((<+>)) where\nimport Control.Arrow\ninfixr 3 <+>\n(<+>) :: Int -> Int -> Int\na <+> b = a + b\n"
      `shouldReturn` Just (Fixity RightAssoc 3)

  it "is not answered at all when the module it came from cannot be read" $
    chased "module M ((<+>)) where\nimport No.Such.Module\n"
      `shouldReturn` Nothing

-- | What the chase makes of one module's @<+>@, against a world of modules
-- that disagree about it.
chased :: Text -> IO (Maybe Fixity)
chased source = do
  answer <- withReexports reach Set.empty "M" (pmModule parsed)
  pure (Map.lookup (OpName "<+>") =<< answer)
  where
    parsed = case parseModule defaultParserConfig "M.hs" source of
      Left _ -> error "the test input did not parse"
      Right m -> m
    reach m =
      pure $ case m of
        "Control.Arrow" -> Just (Map.fromList [(OpName "<+>", Fixity RightAssoc 5)])
        "Text.PrettyPrint" -> Just (Map.fromList [(OpName "<+>", Fixity LeftAssoc 6)])
        "Prelude" -> Just Map.empty
        _ -> Nothing

withPlan :: BuildPlan -> Spec
withPlan plan = do
  resolve <- runIO (newResolver plan)

  describe "the plan itself" $ do
    it "names the compiler" $
      T.unpack (bpCompiler plan) `shouldSatisfy` isInfixOf "ghc-"

    it "has the dependencies a real project has" $
      length (bpPackages plan) `shouldSatisfy` (> 20)

    it "gives every fetchable package a source hash to check against" $ do
      let fetchable = filter isFetchable (bpPackages plan)
      filter (null . sourceHashOf) fetchable `shouldBe` []

    it "does not mark the project itself as fetchable" $ do
      let locals = filter (\p -> ppName p == "tilia") (bpPackages plan)
      filter isFetchable locals `shouldBe` []

    it "records the project as a local directory, with its path" $ do
      let locals = [s' | p <- bpPackages plan, ppName p == "tilia", let s' = ppSource p]
      locals `shouldSatisfy` all (\s' -> case s' of LocalPackage path -> not (null path); _ -> False)

    it "puts every package in exactly one of the three kinds" $ do
      let kinds p = length (filter id [isPreExisting p, isFetchable p, isLocal p])
          isPreExisting p = ppSource p == PreExisting
          isLocal p = case ppSource p of LocalPackage _ -> True; _ -> False
      filter ((/= 1) . kinds) (bpPackages plan) `shouldBe` []

  describe "resolving a module that declares its own operators" $ do
    it "finds <+> in prettyprinter, with the right fixity" $
      needs resolve "Prettyprinter.Internal" $ \fixities ->
        Map.lookup (OpName "<+>") fixities `shouldBe` Just (Fixity RightAssoc 6)

    it "resolves the same module twice to the same answer" $
      needs resolve "Prettyprinter.Internal" $ \first' -> do
        again <- resolve "Prettyprinter.Internal"
        again `shouldBe` Just first'

  describe "re-exports" $
    it "finds an operator a module exports but does not declare" $
      needs resolve "Prettyprinter" $ \fixities ->
        Map.lookup (OpName "<+>") fixities `shouldBe` Just (Fixity RightAssoc 6)

  describe "boot packages" $ do
    it "answers for Prelude from the built-in table" $
      needs resolve "Prelude" $ \fixities -> do
        Map.lookup (OpName "$") fixities `shouldBe` Just (Fixity RightAssoc 0)
        Map.lookup (OpName ">>=") fixities `shouldBe` Just (Fixity LeftAssoc 1)
        Map.lookup (OpName ".") fixities `shouldBe` Just (Fixity RightAssoc 9)
        Map.lookup (OpName ":") fixities `shouldBe` Just (Fixity RightAssoc 5)

    it "answers for Control.Applicative" $
      needs resolve "Control.Applicative" $ \fixities ->
        Map.lookup (OpName "<|>") fixities `shouldBe` Just (Fixity LeftAssoc 3)

    it "covers the containers and text modules a project actually imports" $ do
      let expected =
            [ ("Data.Map", "!", Fixity LeftAssoc 9),
              ("Data.Map", "\\\\", Fixity LeftAssoc 9),
              ("Data.Set", "\\\\", Fixity LeftAssoc 9),
              ("Data.Sequence", "|>", Fixity LeftAssoc 5),
              ("Data.Sequence", "<|", Fixity RightAssoc 5),
              ("Data.Bits", ".&.", Fixity LeftAssoc 7),
              ("Data.Ratio", "%", Fixity LeftAssoc 7),
              ("Data.Functor", "<&>", Fixity LeftAssoc 1),
              ("Control.Monad", ">=>", Fixity RightAssoc 1),
              ("Data.Semigroup", "<>", Fixity RightAssoc 6)
            ]
      wrong <- traverse (check resolve) expected
      concat wrong `shouldBe` []

    it "carries re-exports already resolved" $ do
      p <- resolve "Prelude"
      m <- resolve "Data.Map"
      ( Map.lookup (OpName "$") =<< p,
        Map.lookup (OpName "!") =<< m
        )
        `shouldBe` (Just (Fixity RightAssoc 0), Just (Fixity LeftAssoc 9))

    it "gives the same operator different fixities in different modules" $ do
      inList <- resolve "Data.List"
      inMap <- resolve "Data.Map"
      ( Map.lookup (OpName "\\\\") =<< inList,
        Map.lookup (OpName "\\\\") =<< inMap
        )
        `shouldBe` (Just (Fixity NoAssoc 5), Just (Fixity LeftAssoc 9))

  describe "modules it cannot answer for" $ do
    it "says so rather than claiming no operators" $
      resolve "Not.A.Real.Module.At.All" `shouldReturn` Nothing

    it "says so for a module no package exposes" $
      resolve "Some.Package.That.Does.Not.Exist" `shouldReturn` Nothing

    it "distinguishes a boot module with no operators from an unknown one" $ do
      quiet <- resolve "Data.Char"
      quiet `shouldBe` Just Map.empty

  describe "modules whose source defeats us" $ do
    it "answers for Test.QuickCheck.Property, which cannot be parsed" $
      needs resolve "Test.QuickCheck.Property" $ \fixities -> do
        Map.lookup (OpName "===") fixities `shouldBe` Just (Fixity NoAssoc 4)
        Map.lookup (OpName ".&&.") fixities `shouldBe` Just (Fixity RightAssoc 1)
        Map.lookup (OpName "==>") fixities `shouldBe` Just (Fixity RightAssoc 0)

    it "carries that through the re-export chain to Test.QuickCheck" $
      needs resolve "Test.QuickCheck" $ \fixities ->
        Map.lookup (OpName "===") fixities `shouldBe` Just (Fixity NoAssoc 4)

  describe "a module with more than one configuration" $ do
    -- Built in a temporary directory with a build plan written by hand, so
    -- that the shapes below can be exactly the shapes worth testing. These
    -- are the ones criterion's dependencies turned out to be written in.
    it "answers from the configurations it can read" $
      withFakeProject
        [ ( "src/Platform.hs",
            T.unlines
              [ "{-# LANGUAGE CPP #-}",
                "module Platform (sort, (<+>)) where",
                "#ifdef WINDOWS",
                "import No.Such.Module.At.All",
                "#endif",
                "import Data.List (sort)",
                "infixl 6 <+>",
                "(<+>) :: Int -> Int -> Int",
                "a <+> b = a + b"
              ]
          )
        ]
        $ \ask ->
          -- The WINDOWS branch imports a module nothing has, which is what
          -- System.IO.CodePage does with System.Win32.CodePage. That branch
          -- is passed over rather than taken as a reason to say nothing.
          ask "Platform"
            >>= (`shouldBe` Just (Map.singleton (OpName "<+>") (Fixity LeftAssoc 6)))

    it "still refuses when the configurations it can read disagree" $
      withFakeProject
        [ ( "src/Disagree.hs",
            T.unlines
              [ "{-# LANGUAGE CPP #-}",
                "module Disagree (sort, (<+>)) where",
                "import Data.List (sort)",
                "#ifdef FAST",
                "infixl 6 <+>",
                "#else",
                "infixr 7 <+>",
                "#endif",
                "(<+>) :: Int -> Int -> Int",
                "a <+> b = a + b"
              ]
          )
        ]
        $ \ask -> ask "Disagree" `shouldReturn` Nothing

    it "says nothing when it can read no configuration at all" $
      withFakeProject
        [ ( "src/Bothbad.hs",
            T.unlines
              [ "{-# LANGUAGE CPP #-}",
                "module Bothbad (sort) where",
                "#ifdef WINDOWS",
                "import No.Such.One",
                "#else",
                "import No.Such.Two",
                "#endif",
                "import Data.List (sort)"
              ]
          )
        ]
        $ \ask ->
          -- Not @Just mempty@: that would be claiming the module declares
          -- nothing, which is a guess rather than the silence it deserves.
          ask "Bothbad" `shouldReturn` Nothing

  describe "modules that re-export one another" $
    it "answers for one whose re-exports are mutually entangled" $ do
      answer <- resolve "GHC.Hs"
      answer `shouldSatisfy` (/= Nothing)

  describe "the whole pipeline, from source text to a fixity" $ do
    it "resolves an operator through a real import" $
      endToEnd resolve "module M where\nimport Prettyprinter\n" $ \scope ->
        lookupFixity scope Nothing (OpName "<+>")
          `shouldBe` Resolved (Fixity RightAssoc 6) (DeclaredIn "Prettyprinter")

    it "prefers the module's own declaration to an imported one" $
      endToEnd resolve "module M where\nimport Prettyprinter\ninfixl 2 <+>\n" $ \scope ->
        lookupFixity scope Nothing (OpName "<+>")
          `shouldBe` Resolved (Fixity LeftAssoc 2) DeclaredHere

    it "honours a qualified import" $
      endToEnd resolve "module M where\nimport qualified Prettyprinter as P\n" $ \scope -> do
        lookupFixity scope (Just "P") (OpName "<+>")
          `shouldBe` Resolved (Fixity RightAssoc 6) (DeclaredIn "Prettyprinter")
        -- Qualified-only, so nothing arrives unqualified.
        lookupFixity scope Nothing (OpName "<+>")
          `shouldBe` Resolved defaultFixity ReportDefault

    it "honours an explicit import list" $
      endToEnd resolve "module M where\nimport Prettyprinter ((<+>))\n" $ \scope ->
        lookupFixity scope Nothing (OpName "<+>")
          `shouldBe` Resolved (Fixity RightAssoc 6) (DeclaredIn "Prettyprinter")

    it "honours a hiding list" $
      endToEnd resolve "module M where\nimport Prettyprinter hiding ((<+>))\n" $ \scope ->
        lookupFixity scope Nothing (OpName "<+>")
          `shouldBe` Resolved defaultFixity ReportDefault

    it "concludes the Report default when everything in scope was read" $
      endToEnd resolve "module M where\nimport Prettyprinter\n" $ \scope ->
        lookupFixity scope Nothing (OpName "<!@#>")
          `shouldBe` Resolved defaultFixity ReportDefault

    it "refuses to conclude anything when an import could not be read" $
      endToEnd resolve "module M where\nimport No.Such.Module\n" $ \scope ->
        lookupFixity scope Nothing (OpName "<!@#>")
          `shouldBe` Unresolved ("No.Such.Module" :| [])

    it "still answers for what it did find, despite an unreadable import" $
      endToEnd resolve "module M where\nimport Prettyprinter\nimport No.Such.Module\n" $ \scope ->
        lookupFixity scope Nothing (OpName "<+>")
          `shouldBe` Resolved (Fixity RightAssoc 6) (DeclaredIn "Prettyprinter")

    it "concludes the default through a boot import that exports no operators" $
      endToEnd resolve "module M where\nimport Data.Char\n" $ \scope ->
        lookupFixity scope Nothing (OpName "<!@#>")
          `shouldBe` Resolved defaultFixity ReportDefault

    it "resolves an operator imported from a boot package" $
      endToEnd resolve "module M where\nimport Data.Map\n" $ \scope ->
        lookupFixity scope Nothing (OpName "!")
          `shouldBe` Resolved (Fixity LeftAssoc 9) (DeclaredIn "Data.Map")

    it "reports no ambiguity for a module that compiles" $
      endToEnd resolve "module M where\nimport Prettyprinter\n" $ \scope ->
        scopeAmbiguous scope `shouldBe` []

  describe "readiness" $ do
    it "reports something other than a missing plan for this project" $ do
      readiness <- checkReadiness "."
      readiness `shouldNotBe` PlanMissing

    it "reports a missing plan for a directory that has none" $
      checkReadiness "/" `shouldReturn` PlanMissing

----------------------------------------------------------------------------
-- Helpers

-- | An empty project directory, holding a plan where @cabal@ writes one if
-- it is to hold a plan at all.
withTempProject :: Maybe Text -> (FilePath -> IO a) -> IO a
withTempProject plan act =
  withSystemTempDirectory "tilia-prepare" $ \dir -> do
    createDirectoryIfMissing True (takeDirectory (planPathFor dir))
    traverse_ (writePlan dir) plan
    act dir

writePlan :: FilePath -> Text -> IO ()
writePlan dir = T.writeFile (planPathFor dir)

-- | A plan naming one package that no package cache can have a tarball
-- for, so that reading it leaves something to fetch.
wantingATarball :: Text
wantingATarball =
  "{\"compiler-id\":\"ghc-0.0\",\"install-plan\":\
  \[{\"pkg-name\":\"tilia-phantom\",\"pkg-version\":\"9.9.9\",\
  \\"pkg-src\":{\"type\":\"repo-tar\"}}]}"

-- | Note that @cabal@ was asked for something.
record :: IORef [[String]] -> [String] -> IO ()
record steps args = modifyIORef' steps (<> [args])

-- | A @cabal@ that does nothing and says it went well.
obliging :: IORef [[String]] -> [String] -> IO (Either Text ())
obliging steps args = record steps args >> pure (Right ())

-- | Run an assertion on a module's fixities, or mark the test pending if
-- the module could not be resolved at all.
--
-- Pending rather than failing, because an unpopulated package cache is an
-- environment problem and not a defect in the code under test.
needs ::
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  Text ->
  (Map OpName Fixity -> Expectation) ->
  Expectation
needs resolve modName assertion =
  resolve modName >>= \case
    Nothing -> pendingWith ("could not resolve " <> T.unpack modName)
    Just fixities -> assertion fixities

-- | Parse a module, resolve its imports for real, and hand over the scope.
endToEnd ::
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  Text ->
  (Scope -> Expectation) ->
  Expectation
endToEnd resolve source assertion =
  case parseModule defaultParserConfig "test.hs" source of
    Left _ -> expectationFailure "the test input did not parse"
    Right pm -> do
      scope <- scopeFor resolve (pmModule pm)
      assertion scope

-- | Check one expected fixity, returning a description of any mismatch.
check ::
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  (Text, Text, Fixity) ->
  IO [String]
check resolve (modName, op, expected) = do
  got <- resolve modName
  let actual = Map.lookup (OpName op) =<< got
  pure
    [ T.unpack modName <> "." <> T.unpack op
        <> ": expected "
        <> show expected
        <> " but got "
        <> show actual
    | actual /= Just expected
    ]

-- | A project of made-up modules, with a build plan written by hand.
--
-- The plan names one local package and nothing else, which is enough for a
-- resolver: local modules are read straight off disk, and everything they
-- import here is either a boot module or does not exist.
withFakeProject :: [(FilePath, Text)] -> ((Text -> IO (Maybe (Map OpName Fixity))) -> IO a) -> IO a
withFakeProject sources act =
  withSystemTempDirectory "tilia-plan" $ \dir -> do
    createDirectoryIfMissing True (dir </> "src")
    T.writeFile (dir </> "fake.cabal") $
      T.unlines
        [ "cabal-version: 2.4",
          "name: fake",
          "version: 0.1.0.0",
          "library",
          "  exposed-modules: " <> T.intercalate ", " (map named sources),
          "  hs-source-dirs: src",
          "  default-language: Haskell2010"
        ]
    traverse_ (\(path, text) -> T.writeFile (dir </> path) text) sources
    T.writeFile (dir </> "plan.json") $
      "{\"compiler-id\":\"ghc-0.0\",\"install-plan\":\
      \[{\"pkg-name\":\"fake\",\"pkg-version\":\"0.1.0.0\",\
      \\"pkg-src\":{\"type\":\"local\",\"path\":\""
        <> T.pack dir
        <> "\"}}]}"
    readBuildPlan (dir </> "plan.json") >>= \case
      Left why -> error (T.unpack why)
      Right plan -> newResolver plan >>= act
  where
    named (path, _) = T.pack (takeBaseName path)

-- | Say why nothing could be tested, once, instead of failing repeatedly.
unavailable :: String -> Spec
unavailable reason =
  it "needs a built project" $ pendingWith reason
