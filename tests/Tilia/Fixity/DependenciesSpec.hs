{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | The fixity machinery, run over every dependency this project has.
--
-- "Tilia.Fixity.PlanSpec" checks the pipeline on a handful of modules
-- picked for what each one exercises. This checks it on all of them. Every
-- module of every package in this project's build plan is read out of the
-- package's source tarball and compared against what the compiler recorded
-- when it built that same package: two independent readings of one fact,
-- one by us and one by GHC.
module Tilia.Fixity.DependenciesSpec (spec) where

import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.List (isSuffixOf, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Hs (HsModule)
import GHC.Hs.Extension (GhcPs)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Tilia.Fixity
import Tilia.Fixity.Builtin (builtinFixities)
import Tilia.Fixity.Interface (Interface (..), readInterface)
import Tilia.Fixity.PackageDb
import Tilia.Fixity.Plan
import Tilia.Parser

spec :: Spec
spec = do
  plan <- runIO (readBuildPlan (planPathFor "."))
  case plan of
    Left _ ->
      it "needs a built project" $
        pendingWith "no build plan; run cabal build first"
    Right p -> withPlan p

withPlan :: BuildPlan -> Spec
withPlan plan = do
  installed <- runIO readInstalledPackages
  fromSource <- runIO (newResolverVia [FromSource] plan)
  fromInterface <- runIO (newResolverVia [FromInterface] plan)
  resolve <- runIO (newResolver plan)
  own <- runIO ownModules
  let dependencies = dependenciesOf (not . shipped) plan installed
      preloaded = dependenciesOf shipped plan installed
      shipped m = Map.member m builtinFixities
      modules = concatMap depModules dependencies
  -- Measured here rather than inside a test. By the time the tests run, the
  -- sweeps below have been at the same resolver from a dozen threads at
  -- once, and a module caught in a re-export cycle can come out differently
  -- depending on which thread reached it first: the same sweep measured
  -- 80% alone and 74% amid the others. Read once, sequentially, before any
  -- of that, and the number is the same every run.
  fromSourceAnswers <- runIO (traverse (fromSource . fst) modules)

  describe "the tree this project is built against" $ do
    it "is a real dependency tree and not an empty plan" $
      length dependencies `shouldSatisfy` (>= 30)

    it "holds modules the compiler does not ship a fixity table for" $
      length modules `shouldSatisfy` (>= 500)

  describe "the operators the compiler ships with" $
    parallel $ for_ (concatMap testsFor preloaded) $ \(label, chunk) ->
      it label $ do
        wrong <- traverse contradicts chunk
        concat wrong `shouldBe` []

  describe "every dependency declares what the compiler recorded" $
    parallel $ for_ (concatMap testsFor dependencies) $ \(label, chunk) ->
      it label $ do
        wrong <- traverse (undeclared fromSource) chunk
        concat wrong `shouldBe` []

  describe "every dependency reads the same both ways" $
    parallel $ for_ (concatMap testsFor dependencies) $ \(label, chunk) ->
      it label $ do
        wrong <- traverse (conflicting fromSource fromInterface . fst) chunk
        concat wrong `shouldBe` []

  describe "how much of the tree it reaches" $ do
    it "answers for every module of every dependency" $ do
      answers <- traverse (\(m, _) -> (m,) <$> resolve m) modules
      [m | (m, Nothing) <- answers] `shouldBe` []

    it "answers for every one of them out of the interfaces alone" $ do
      answers <- traverse (\(m, _) -> (m,) <$> fromInterface m) modules
      [m | (m, Nothing) <- answers] `shouldBe` []

    it "reads three quarters of them out of source alone" $ do
      let reached = length [() | Just _ <- fromSourceAnswers]
      percent reached (length modules) `shouldSatisfy` (>= 75)

    it "finds the operators that are in it" $
      sum [Map.size fixities | Just fixities <- fromSourceAnswers] `shouldSatisfy` (>= 300)

  describe "this project's own modules" $ do
    it "parses every one of them" $
      [path | (path, Nothing) <- own] `shouldBe` []

    it "resolves every module they import" $ do
      answers <- traverse (\m -> (m,) <$> resolve m) (importedByOwn own)
      [m | (m, Nothing) <- answers] `shouldBe` []

    it "settles every operator they use" $ do
      unsettled <- traverse (unsettledIn resolve) [(path, m) | (path, Just m) <- own]
      concat unsettled `shouldBe` []

----------------------------------------------------------------------------
-- The dependencies

-- | A package this project is built against, as the compiler holds it.
data Dependency = Dependency
  { -- | The package name
    depPackage :: Text,
    -- | Each module the package holds, with the interface file the compiler
    -- wrote for it.
    depModules :: [(Text, FilePath)]
  }

-- | The modules of every package in the plan that the compiler can also
-- see, keeping the ones the predicate wants.
dependenciesOf :: (Text -> Bool) -> BuildPlan -> Installed -> [Dependency]
dependenciesOf wanted plan installed =
  [ Dependency (ipName package) modules
  | package <- installedPackages installed,
    Set.member (ipName package) planned,
    dir <- take 1 (ipImportDirs package),
    let modules =
          [ (m, dir </> T.unpack (T.replace "." "/" m) <> ".hi")
          | m <- ipModules package,
            wanted m
          ],
    not (null modules)
  ]
  where
    planned = Set.fromList [ppName p | p <- bpPackages plan, not (isLocal p)]
    isLocal p = case ppSource p of
      LocalPackage _ -> True
      _ -> False

-- | One test per package, splitting the large ones up.
testsFor :: Dependency -> [(String, [(Text, FilePath)])]
testsFor dependency = case chunksOf 32 (depModules dependency) of
  [whole] -> [(name, whole)]
  pieces ->
    [ (name <> " (" <> show i <> " of " <> show (length pieces) <> ")", piece)
    | (i, piece) <- zip [1 :: Int ..] pieces
    ]
  where
    name = T.unpack (depPackage dependency)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = \case
  [] -> []
  xs -> let (chunk, rest) = splitAt n xs in chunk : chunksOf n rest

-- | Where the built-in table and the compiler both hold a fixity for an
-- operator and it is not the same fixity.
--
-- The table in "Tilia.Fixity.Builtin" was written by asking a GHC of one
-- version what its boot packages export. The tests run against whichever
-- GHC built the project, which this package supports three of. This is what
-- says the answer has not moved underneath the table.
contradicts :: (Text, FilePath) -> IO [String]
contradicts (modName, interfaceFile) =
  readInterface modName interfaceFile >>= \case
    Nothing -> pure []
    Just interface ->
      pure
        [ T.unpack modName
            <> ": "
            <> show op
            <> " is "
            <> show declared
            <> " per the compiler, "
            <> show ours
            <> " in the table"
        | (op, declared) <- Map.toList (interfaceDeclares interface),
          Just ours <- [Map.lookup op table],
          ours /= declared
        ]
  where
    table = Map.findWithDefault Map.empty modName builtinFixities

-- | Every fixity the compiler recorded for a module that reading the
-- package's source did not produce.
undeclared ::
  -- | What a module declares, read from the package's source
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  -- | The module, and the interface the compiler wrote for it
  (Text, FilePath) ->
  IO [String]
undeclared fromSource (modName, interfaceFile) =
  readInterface modName interfaceFile >>= \case
    Nothing -> pure []
    Just interface ->
      fromSource modName >>= \case
        Nothing -> pure []
        Just fixities ->
          pure
            [ T.unpack modName
                <> ": "
                <> show op
                <> " is "
                <> show declared
                <> " per the compiler, "
                <> show (Map.lookup op fixities)
                <> " from source"
            | (op, declared) <- Map.toList (interfaceDeclares interface),
              writable declared,
              Map.lookup op fixities /= Just declared
            ]
  where
    -- GHC files @->@ under a module's fixities at precedence -1, below
    -- anything a source file is allowed to declare.
    writable f = fixityPrecedence f >= 0 && fixityPrecedence f <= 9

-- | Where the two routes both have an answer for an operator and it is not
-- the same answer.
--
-- Wider than 'undeclared', because a module's own declarations are the
-- smaller part of what it offers: most operators reach the module that
-- exports them through a chain of re-exports, and following that chain
-- through source text is the part of this most likely to go wrong. The
-- compiler followed the same chain when it built the package, so the two
-- have to arrive at the same place.
conflicting ::
  -- | The answer read out of the package's source
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  -- | The answer read out of the compiler's interfaces
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  Text ->
  IO [String]
conflicting fromSource fromInterface modName = do
  source <- fromSource modName
  compiled <- fromInterface modName
  pure $ case (source, compiled) of
    (Just a, Just b) ->
      [ T.unpack modName
          <> ": "
          <> show op
          <> " is "
          <> show fromText
          <> " from source, "
          <> show fromIface
          <> " from the interface"
      | (op, (fromText, fromIface)) <- Map.toList (Map.intersectionWith (,) a b),
        fromText /= fromIface
      ]
    _ -> []

percent :: Int -> Int -> Int
percent part whole = if whole == 0 then 0 else part * 100 `div` whole

----------------------------------------------------------------------------
-- This project

-- | Every Haskell file this project is made of, parsed.
--
-- 'Nothing' where one did not parse, which is a failure of its own rather
-- than something to skip over quietly.
ownModules :: IO [(FilePath, Maybe (HsModule GhcPs))]
ownModules = do
  paths <- concat <$> traverse haskellFilesIn ["src", "app", "tests"]
  traverse parsed paths
  where
    parsed path = do
      source <- T.decodeUtf8Lenient <$> BS.readFile path
      pure
        ( path,
          case parseModule defaultParserConfig path source of
            Left _ -> Nothing
            Right pm -> Just (pmModule pm)
        )

haskellFilesIn :: FilePath -> IO [FilePath]
haskellFilesIn dir = do
  entries <- sort <$> listDirectory dir
  concat <$> traverse below entries
  where
    below entry = do
      let path = dir </> entry
      isDir <- doesDirectoryExist path
      if isDir
        then haskellFilesIn path
        else pure [path | ".hs" `isSuffixOf` path]

-- | Every module this project's own source imports.
importedByOwn :: [(FilePath, Maybe (HsModule GhcPs))] -> [Text]
importedByOwn own =
  Set.toList . Set.fromList $
    [ importModule i
    | (_, Just hsModule) <- own,
      i <- moduleImports hsModule,
      not ("Paths_" `T.isPrefixOf` importModule i)
    ]

-- | The operators one of this project's modules uses that its imports,
-- resolved for real, cannot settle.
--
-- This is the whole machinery end to end: the plan is read, the packages
-- are found, their modules are read, the scope is assembled and the
-- operators are looked up in it. Anything left over is an operator this
-- project could not be laid out from.
unsettledIn ::
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  (FilePath, HsModule GhcPs) ->
  IO [String]
unsettledIn resolve (path, hsModule) = do
  scope <- scopeFor resolve hsModule
  pure
    [ path <> ": " <> show op <> " " <> show why
    | (op, why) <- unknownOperators scope hsModule
    ]
