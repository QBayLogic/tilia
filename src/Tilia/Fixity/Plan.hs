{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | "Tilia.Fixity" resolves a module's operators exactly, given a function
-- that says what each imported module exports. This is that function, built
-- from what the project itself is compiled against.
module Tilia.Fixity.Plan
  ( -- * Build plans
    PlanPackage (..),
    PackageSource (..),
    isFetchable,
    sourceHashOf,
    BuildPlan (..),
    readBuildPlan,

    -- * Readiness
    Readiness (..),
    PlanComponent (..),
    spellComponent,
    plannedComponents,
    planPathFor,
    checkReadiness,
    plannedTarballs,
    prepareWith,
    loadPlan,

    -- * Resolving
    Route (..),
    Resolver (..),
    newResolver,
    newResolverVia,
    withReexports,
    scopeFor,
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Monad (filterM, foldM, join)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.:), (.:?))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.IORef
import Data.List (isSuffixOf)
import Data.List qualified
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Hs (HsModule)
import GHC.Hs.Extension (GhcPs)
import GHC.IO.Handle (hDuplicate)
import GHC.LanguageExtensions.Type (Extension)
import System.Directory
  ( doesFileExist,
    getHomeDirectory,
    getModificationTime,
    listDirectory,
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hFlush, stderr)
import System.Process
  ( StdStream (Inherit, UseHandle),
    createProcess,
    cwd,
    proc,
    std_err,
    std_out,
    waitForProcess,
  )
import Tilia.Cpp (branchLeaves)
import Tilia.Fixity
import Tilia.Fixity.Builtin (builtinFixities)
import Tilia.Fixity.ByHand (byHandFixities)
import Tilia.Fixity.Cabal
  ( containedModules,
    declaredExtensions,
    findCabalFile,
    packageModules,
    sourceDirs,
  )
import Tilia.Fixity.Cache
import Tilia.Fixity.Interface
import Tilia.Fixity.PackageDb
import Tilia.Package (newPackageReader)
import Tilia.Parser
import Tilia.Pragma (effectiveExtensions)
import Tilia.Utils (quietly)

----------------------------------------------------------------------------
-- The plan

-- | One package of a build plan.
data PlanPackage = PlanPackage
  { -- | Package name
    ppName :: Text,
    -- | Package version
    ppVersion :: Text,
    -- | Package source
    ppSource :: PackageSource,
    -- | Which component of the package the entry is about
    ppComponent :: Maybe Text
  }
  deriving (Eq, Show)

-- | Where a package's source is, if anywhere.
--
-- A plan contains exactly three kinds of entry and they are mutually
-- exclusive, which two independent flags could not say: a package cannot be
-- both shipped with the compiler and fetched from Hackage. Each carries
-- what is peculiar to it and nothing else, so there is no hash to consult
-- on a package that has no tarball, and no tarball to look for on one that
-- is a directory.
data PackageSource
  = -- | Already installed, so @cabal@ will not build it.
    --
    -- Not the same as "ships with the compiler", though it includes those.
    PreExisting
  | -- | A directory on this machine—the project being formatted, or a
    -- sibling of it in the same repository.
    LocalPackage FilePath
  | -- | Fetched from Hackage as a tarball, with the SHA-256 the plan
    -- expects it to have.
    --
    -- The hash is optional only because a plan is not obliged to record
    -- one; every entry in a plan @cabal@ writes for a secure repository
    -- does.
    HackagePackage (Maybe Text)
  deriving (Eq, Show)

-- | Is there a tarball to go and read?
isFetchable :: PlanPackage -> Bool
isFetchable p = case ppSource p of
  HackagePackage _ -> True
  _ -> False

-- | Every package the compiler can see.
whatTheCompilerSees :: Maybe Cache -> IO [InstalledPackage]
whatTheCompilerSees cache =
  remembered >>= \case
    Just packages -> pure packages
    Nothing -> do
      found <- readInstalledPackages
      traverse_ (`storeInstalled` found) cache
      pure (installedPackages found)
  where
    remembered = maybe (pure Nothing) cachedInstalled cache

-- | Summarize a 'BuildPlan' by hashing over it.
planToken :: BuildPlan -> PlanToken
planToken plan =
  PlanToken
    . T.take 16
    . T.decodeUtf8Lenient
    . B16.encode
    . SHA256.hash
    . T.encodeUtf8
    $ T.intercalate
      "\n"
      (bpCompiler plan : Data.List.sort (map cacheKey (bpPackages plan)))

-- | The SHA-256 the plan expects this package's tarball to have.
sourceHashOf :: PlanPackage -> Maybe Text
sourceHashOf p = case ppSource p of
  HackagePackage hash -> hash
  _ -> Nothing

-- | A resolved build plan.
data BuildPlan = BuildPlan
  { bpCompiler :: Text,
    bpPackages :: [PlanPackage]
  }
  deriving (Eq, Show)

instance FromJSON BuildPlan where
  parseJSON = withObject "BuildPlan" $ \o ->
    BuildPlan
      <$> o .: "compiler-id"
      <*> o .: "install-plan"

instance FromJSON PlanPackage where
  parseJSON = withObject "PlanPackage" $ \o -> do
    name <- o .: "pkg-name"
    version <- o .: "pkg-version"
    kind <- o .:? "type"
    sourceKind <- o .:? "pkg-src" >>= traverse (.: "type")
    sourcePath <- o .:? "pkg-src" >>= traverse (.:? "path")
    sourceHash <- o .:? "pkg-src-sha256"
    component <- o .:? "component-name"
    pure
      PlanPackage
        { ppName = name,
          ppVersion = version,
          ppComponent = component,
          ppSource = case (kind :: Maybe Text, sourceKind :: Maybe Text) of
            (Just "pre-existing", _) -> PreExisting
            (_, Just "repo-tar") -> HackagePackage sourceHash
            (_, Just "local") -> LocalPackage (maybe "" T.unpack (join sourcePath))
            -- Anything else is treated as already present.
            _ -> PreExisting
        }

-- | Read @plan.json@.
readBuildPlan :: FilePath -> IO (Either Text BuildPlan)
readBuildPlan path =
  doesFileExist path >>= \case
    False -> pure (Left ("no build plan at " <> T.pack path))
    True -> either (Left . T.pack) Right <$> eitherDecodeFileStrict path

----------------------------------------------------------------------------
-- Readiness

-- | A component of the project, named the way a build plan names one.
data PlanComponent = PlanComponent
  { -- | The package it belongs to.
    pcPackage :: Text,
    -- | @lib@, @exe:name@, @test:name@, @bench:name@.
    pcName :: Text
  }
  deriving (Eq, Ord, Show)

-- | A component as it would be written on the command line.
spellComponent :: PlanComponent -> Text
spellComponent c = pcPackage c <> ":" <> pcName c

-- | The components of the project's own packages that a plan covers.
plannedComponents :: BuildPlan -> [PlanComponent]
plannedComponents plan =
  [ PlanComponent (ppName p) component
  | p <- bpPackages plan,
    LocalPackage _ <- [ppSource p],
    Just component <- [ppComponent p]
  ]

-- | Whether everything the resolver needs is on disk.
data Readiness
  = -- | Nothing to do.
    Ready
  | -- | No build plan; @cabal@ has not solved this project yet.
    PlanMissing
  | -- | The plan is older than the files that determine it.
    PlanStale [FilePath]
  | -- | The plan says nothing about components the run is about to format.
    PlanNarrow [Text]
  | -- | The plan is there, and some packages have neither been downloaded
    -- nor built. The names are listed so that a caller can say what it is
    -- waiting for.
    --
    -- Built counts as having them: their interfaces answer everything the
    -- source would have been read for, so a package the compiler already
    -- holds is not missing however absent its tarball is.
    SourcesMissing [Text]
  deriving (Eq, Show)

-- | Where @cabal@ writes the plan for a project.
planPathFor :: FilePath -> FilePath
planPathFor projectDir = projectDir </> "dist-newstyle" </> "cache" </> "plan.json"

-- | Check what is missing, cheaply.
--
-- One read of the plan and one @stat@ per package, so this is fast enough
-- to run before every format without anyone noticing.
checkReadiness :: [PlanComponent] -> FilePath -> IO Readiness
checkReadiness wanted projectDir =
  readBuildPlan (planPathFor projectDir) >>= \case
    Left _ -> pure PlanMissing
    Right plan -> do
      newer <- filesNewerThanPlan projectDir
      let covered = plannedComponents plan
          missing = [spellComponent c | c <- wanted, c `notElem` covered]
      case (newer, missing) of
        (_ : _, _) -> pure (PlanStale newer)
        ([], _ : _) -> pure (PlanNarrow missing)
        ([], []) -> do
          tarballs <- filter (isFetchable . fst) <$> plannedTarballs plan
          absent <- map fst <$> filterM (fmap not . doesFileExist . snd) tarballs
          short <-
            if null absent
              then pure []
              else do
                cache <- openCache (planToken plan)
                installed <- whatTheCompilerSees cache
                pure (filter (not . builtAlready installed) absent)
          pure $ case map ppName short of
            [] -> Ready
            ns -> SourcesMissing ns

-- | Has the compiler got this package already?
builtAlready :: [InstalledPackage] -> PlanPackage -> Bool
builtAlready installed p = any matches installed
  where
    matches i = ipName i == ppName p && ipVersion i == ppVersion p

-- | The project files that have changed since the plan was written.
--
-- A plan describes the dependencies as they were when @cabal@ last solved.
-- Edit a @build-depends@ and the plan on disk is about a different project,
-- and resolving fixities against it would answer for packages that are no
-- longer in play. Comparing modification times is one @stat@ each, so this
-- costs nothing to check every time.
filesNewerThanPlan :: FilePath -> IO [FilePath]
filesNewerThanPlan projectDir = quietly [] $ do
  planTime <- getModificationTime (planPathFor projectDir)
  entries <- quietly [] (listDirectory projectDir)
  let candidates =
        filter
          (\f -> f `elem` projectFiles || ".cabal" `isSuffixOf` f)
          entries
  newer <- traverse (isNewerThan planTime) candidates
  pure [f | Just f <- newer]
  where
    projectFiles =
      ["cabal.project", "cabal.project.local", "cabal.project.freeze"]
    isNewerThan planTime f = quietly Nothing $ do
      t <- getModificationTime (projectDir </> f)
      pure (if t > planTime then Just f else Nothing)

-- | Do whatever is missing, by asking @cabal@.
--
-- Neither of these builds anything: a dry run only solves, and
-- @--only-download@ only fetches. Both are one-time costs, and @cabal@'s
-- package cache is shared between projects, so a machine that has seen a
-- dependency once never fetches it again.
--
-- This runs a subprocess and may reach the network, so it is a separate
-- call rather than something 'newResolver' does behind the caller's back.
-- An editor formatting on save must not block on it.
prepare :: [PlanComponent] -> FilePath -> Readiness -> IO (Either Text ())
prepare wanted projectDir = prepareWith (runCabal projectDir) wanted projectDir

-- | 'prepare', given a way to run @cabal@.
prepareWith ::
  -- | Run @cabal@ with these arguments
  ([String] -> IO (Either Text ())) ->
  -- | The components the run is about to format
  [PlanComponent] ->
  -- | The project being prepared
  FilePath ->
  -- | What it was found to be short of
  Readiness ->
  IO (Either Text ())
prepareWith cabal wanted projectDir = \case
  Ready -> pure (Right ())
  SourcesMissing _ -> fetch
  PlanMissing -> solveThenFetch
  PlanStale _ -> solveThenFetch
  PlanNarrow _ -> solveThenFetch
  where
    fetch = cabal ["build", "all", "--only-download"]
    solveThenFetch =
      cabal ["build", "all", "--dry-run"] >>= \case
        Left err -> pure (Left err)
        Right () ->
          checkReadiness wanted projectDir >>= \case
            SourcesMissing _ -> fetch
            -- Either there is nothing left to do, or the plan @cabal@ has
            -- just written is one we still cannot read. A second dry run
            -- would not say anything the first did not.
            _ -> pure (Right ())

-- | Run @cabal@ in a project directory, letting it speak for itself.
runCabal :: FilePath -> [String] -> IO (Either Text ())
runCabal projectDir args = quietly (Left "could not run cabal") $ do
  hFlush stderr
  -- A duplicate because 'createProcess' closes the handle it is given once
  -- the child has it, and closing the real standard error would leave
  -- nothing to report the failure on.
  passed <- hDuplicate stderr
  (_, _, _, running) <-
    createProcess
      (proc "cabal" args)
        { cwd = Just projectDir,
          std_out = UseHandle passed,
          std_err = Inherit
        }
  code <- waitForProcess running
  pure $ case code of
    ExitSuccess -> Right ()
    _ -> Left ("cabal " <> T.unwords (map T.pack args) <> " failed; see above")

-- | Get a plan that is safe to use, doing whatever @cabal@ work is needed.
--
-- This is the call most users want. It checks, asks @cabal@ if anything is
-- missing or possibly out of date, and then reads the plan. 'prepare' does
-- at most one solve and one fetch however much is missing, so a project
-- whose files are merely newer than its plan cannot send this into a loop,
-- and the plan is read once at the end rather than judged again.
loadPlan :: [PlanComponent] -> FilePath -> IO (Either Text BuildPlan)
loadPlan wanted projectDir = do
  readiness <- checkReadiness wanted projectDir
  prepare wanted projectDir readiness >>= \case
    Left err -> pure (Left err)
    _ -> readBuildPlan (planPathFor projectDir)

-- | Every planned package whose source could be in the package cache, with
-- where that would be.
--
-- Not only the ones the plan will fetch. A package already installed still
-- has a tarball in the cache if anything ever downloaded it, and under Nix
-- that is the normal case for every dependency. A package with no tarball
-- costs one @stat@ and falls through.
--
-- Local packages are excluded: they are directories, not archives.
plannedTarballs :: BuildPlan -> IO [(PlanPackage, FilePath)]
plannedTarballs plan = do
  cacheDir <- packageCacheDir
  pure
    [ (p, tarballFor cacheDir p)
    | p <- bpPackages plan,
      not (isLocal p)
    ]
  where
    isLocal p = case ppSource p of
      LocalPackage _ -> True
      _ -> False

-- | Where @cabal@ keeps downloaded package sources.
packageCacheDir :: IO FilePath
packageCacheDir =
  lookupEnv "CABAL_DIR" >>= \case
    Just dir -> pure (dir </> "packages" </> hackage)
    Nothing -> do
      home <- getHomeDirectory
      let xdg = home </> ".cache" </> "cabal" </> "packages" </> hackage
          legacy = home </> ".cabal" </> "packages" </> hackage
      exists <- doesFileExist (xdg </> "01-index.tar")
      pure (if exists then xdg else legacy)
  where
    hackage = "hackage.haskell.org"

-- | Where a package's source tarball should be.
tarballFor :: FilePath -> PlanPackage -> FilePath
tarballFor cacheDir p =
  cacheDir
    </> T.unpack (ppName p)
    </> T.unpack (ppVersion p)
    </> T.unpack (ppName p <> "-" <> ppVersion p <> ".tar.gz")

----------------------------------------------------------------------------
-- Resolving

-- | Where a module's fixities can be read from.
data Route
  = -- | The compiled interface the package database points at. Cheap, and
    -- authoritative where it exists, since it is the compiler's own account
    -- of what it settled on.
    FromInterface
  | -- | The module's source, out of the package's tarball in Cabal's
    -- package cache. Slower, and the only route for a package that is
    -- planned but not built.
    FromSource
  deriving (Eq, Show)

-- | What can be asked about a module, once a plan says where to look.
--
-- The three questions "Tilia.Fixity" has, answered against the outside
-- world. They come back together because they share everything—the module
-- index, the cache, the packages the compiler holds, the memo of what has
-- been read—and answering them apart would settle all of it three times.
data Resolver = Resolver
  { -- | What a module exports, with 'Nothing' for one that could not be
    -- read, which is not the same as its having no operators; see
    -- 'Tilia.Fixity.resolveScope' for why the difference has to survive.
    askFixities :: Text -> IO (Maybe (Map OpName Fixity)),
    -- | What a module keeps under each of its names, for the sake of a
    -- @T(..)@ in an import list.
    askChildren :: Text -> IO (Map OpName (Set OpName)),
    -- | The operators an unread module's export list names, asked only of
    -- the modules 'askFixities' gave up on, and what keeps a module that
    -- plainly has no such operator from being blamed for one.
    askExportNames :: Text -> IO (Maybe (Set OpName))
  }

-- | Build the answers to what "Tilia.Fixity" asks.
--
-- Answers are remembered on disk between runs by "Tilia.Fixity.Cache", so a
-- package is decompressed and parsed once per machine rather than once per
-- file.
newResolver ::
  -- | The build plan to use
  BuildPlan ->
  IO Resolver
newResolver = newResolverVia [FromInterface, FromSource]

-- | 'newResolver', restricted to the routes given.
newResolverVia ::
  -- | Which readings to try, in order
  [Route] ->
  -- | The build plan to use
  BuildPlan ->
  IO Resolver
newResolverVia routes plan = do
  tarballs <- plannedTarballs plan
  cache <- openCache (planToken plan)
  installed <- whatTheCompilerSees cache
  index <- buildModuleIndex cache installed tarballs
  let interfaces = interfaceIndex installed
  local <- localModules plan
  memo <- newIORef Map.empty
  childrenRead <- newIORef Map.empty
  askPackage <- newPackageReader
  extensionsRead <- newIORef Map.empty
  exportsRead <- newIORef Map.empty
  interfacesRead <- newIORef Map.empty
  let interfaceOf modName = do
        seen <- readIORef interfacesRead
        case Map.lookup modName seen of
          Just interface -> pure interface
          Nothing -> do
            found <- case Map.lookup modName interfaces of
              Nothing -> pure Nothing
              Just (_, path) -> readInterface modName path
            -- Being listed is not the same as being readable: @ghc-pkg@
            -- names @GHC.Prim@ among @ghc-prim@'s modules and there is no
            -- file at the path that implies. So the table answers for a
            -- module with nothing to read, however it came to have nothing.
            let interface = case found of
                  Just _ -> found
                  Nothing -> asInterface <$> Map.lookup modName builtinFixities
            atomicModifyIORef' interfacesRead (\m -> (Map.insert modName interface m, ()))
            pure interface
  let workings =
        Workings
          { wkRoutes = routes,
            wkCache = cache,
            wkLocal = local,
            wkIndex = index,
            wkInterfaces = interfaces,
            wkInterfaceOf = interfaceOf,
            wkReach = reach,
            wkReachChildren = children,
            wkReachExports = exports,
            wkExtensionsOf = extensionsOf
          }
      reach visiting modName
        | modName `Set.member` visiting = pure Nothing
        | otherwise = do
            known <- readIORef memo
            case Map.lookup modName known of
              Just answer -> pure answer
              Nothing -> do
                answer <- resolveModule workings visiting modName
                atomicModifyIORef' memo (\m -> (Map.insert modName answer m, ()))
                pure answer
      exports visiting modName
        | modName `Set.member` visiting = pure Nothing
        | otherwise = do
            seen <- readIORef exportsRead
            case Map.lookup modName seen of
              Just names -> pure names
              Nothing -> do
                names <- exportNamesOfModule workings visiting modName
                atomicModifyIORef' exportsRead (\m -> (Map.insert modName names m, ()))
                pure names
      extensionsOf modName
        | Just path <- Map.lookup modName local =
            either (const []) id <$> askPackage path
        | Just (package, tarball) <- Map.lookup modName index = do
            seen <- readIORef extensionsRead
            case Map.lookup package seen of
              Just extensions -> pure extensions
              Nothing -> do
                extensions <- fromTarball tarball
                atomicModifyIORef' extensionsRead (\m -> (Map.insert package extensions m, ()))
                pure extensions
        | otherwise = pure []
      fromTarball tarball =
        quietly [] $ do
          bytes <- BL.readFile tarball
          pure (foldMap declaredExtensions (findCabalFile (Tar.read (GZip.decompress bytes))))
      children visiting modName
        | modName `Set.member` visiting = pure Map.empty
        | otherwise = do
            seen <- readIORef childrenRead
            case Map.lookup modName seen of
              Just kept -> pure kept
              Nothing -> do
                kept <- childrenOfModule workings visiting modName
                atomicModifyIORef' childrenRead (\m -> (Map.insert modName kept m, ()))
                pure kept
  pure
    Resolver
      { askFixities = reach Set.empty,
        askChildren = children Set.empty,
        askExportNames = exports Set.empty
      }

-- | The operators a module's export list names, where that list can be
-- enumerated without reading what it passes on.
--
-- Asked only about modules whose fixities could not be established, and
-- only to decide which of them an unsettled operator can be blamed on. A
-- module that exports whole modules keeps its own counsel and answers
-- 'Nothing'; one with no export list exports what it declares, which is
-- every fixity it could supply.
exportNamesOfModule :: Workings -> Set Text -> Text -> IO (Maybe (Set OpName))
exportNamesOfModule
  Workings {wkCache, wkLocal, wkIndex, wkReachChildren, wkReachExports}
  visiting
  modName
    | Just path <- Map.lookup modName wkLocal = namesIn =<< readFileText path
    | Just (package, tarball) <- Map.lookup modName wkIndex =
        remembered package >>= \case
          Just answer -> pure (exportedNames answer)
          Nothing ->
            readModule tarball modName >>= \case
              Nothing -> pure Nothing
              Just text -> do
                names <- namesIn (Just text)
                store package (asExported names)
                pure names
    | otherwise = pure Nothing
    where
      remembered package = case wkCache of
        Nothing -> pure Nothing
        Just c -> cachedExportNames c package modName
      store package answer = case wkCache of
        Nothing -> pure ()
        Just c -> storeExportNames c package modName answer

      -- Across every configuration the preprocessor allows, since a name
      -- exported under any of them is one the module can supply.
      namesIn text = case parsedLeaves =<< text of
        Nothing -> pure Nothing
        Just modules ->
          fmap Set.unions . sequence
            <$> traverse
              (exportNamesWithReexports (wkReachExports visiting') (wkReachChildren visiting') modName)
              modules
      visiting' = Set.insert modName visiting
      parsedLeaves text = do
        leaves <- either (const Nothing) Just (branchLeaves text)
        traverse parsed leaves
      parsed =
        fmap pmModule
          . either (const Nothing) Just
          . parseModule defaultParserConfig (T.unpack modName)

-- | What a module keeps under each of its names, so that a @T(..)@ in an
-- import list can be told what it brings in.
childrenOfModule :: Workings -> Set Text -> Text -> IO (Map OpName (Set OpName))
childrenOfModule
  Workings
    { wkRoutes,
      wkCache,
      wkLocal,
      wkIndex,
      wkInterfaces,
      wkInterfaceOf,
      wkReachChildren,
      wkExtensionsOf
    }
  visiting
  modName
    | Just path <- Map.lookup modName wkLocal =
        readFileText path >>= \case
          Nothing -> pure Map.empty
          Just text -> inSource text
    | otherwise = firstAnswer (map taking wkRoutes)
    where
      taking = \case
        FromInterface -> case Map.lookup modName wkInterfaces of
          Nothing -> pure Nothing
          Just (key, _) -> keptUnder key outOfInterface
        FromSource -> case Map.lookup modName wkIndex of
          Nothing -> pure Nothing
          Just (package, tarball) ->
            keptUnder package (traverse inSource =<< readModule tarball modName)
      firstAnswer [] = pure Map.empty
      firstAnswer (route : rest) =
        route >>= \case
          Just kept -> pure kept
          Nothing -> firstAnswer rest
      keptUnder package readIt =
        remembered package >>= \case
          Just kept -> pure (Just kept)
          Nothing -> do
            kept <- readIt
            traverse_ (store package) kept
            pure kept
      remembered package = case wkCache of
        Nothing -> pure Nothing
        Just c -> cachedChildren c package modName
      store package kept = case wkCache of
        Nothing -> pure ()
        Just c -> storeChildren c package modName kept
      outOfInterface = fmap interfaceChildren <$> wkInterfaceOf modName
      inSource text = do
        extensions <- wkExtensionsOf modName
        case parsedLeaves extensions text of
          Nothing -> pure Map.empty
          Just modules ->
            Map.unionsWith Set.union
              <$> traverse (childrenWithReexports (wkReachChildren visiting') modName) modules
      visiting' = Set.insert modName visiting
      parsedLeaves extensions text = do
        leaves <- either (const Nothing) Just (branchLeaves text)
        traverse (parsed extensions) leaves
      parsed extensions leaf =
        fmap pmModule
          . either (const Nothing) Just
          $ parseModule (configFor extensions leaf) (T.unpack modName) leaf

-- | Work out what a module can see, using a resolver to reach its imports.
--
-- This is the join between the pure half of "Tilia.Fixity" and the half
-- that touches the disk: the imports are resolved first, and the scope is
-- then computed from the answers. Note that an import the resolver could
-- not read arrives as 'Nothing' and stays 'Nothing', which is what lets
-- 'Tilia.Fixity.lookupFixity' distinguish a conclusion from a guess.
scopeFor ::
  -- | What can be asked about the modules it imports
  Resolver ->
  -- | The module whose scope is wanted, already parsed
  HsModule GhcPs ->
  -- | Everything that module can see, and what it could not find out
  IO Scope
scopeFor resolver hsModule = do
  let imports = moduleImports hsModule
  answers <- traverse (\m -> (m,) <$> askFixities resolver m) (map importModule imports)
  let table = Map.fromList answers
      unread = [m | (m, Nothing) <- answers]
  names <- Map.fromList <$> traverse (\m -> (m,) <$> askExportNames resolver m) unread
  kept <-
    Map.fromList
      <$> traverse
        (\m -> (m,) <$> askChildren resolver m)
        (Set.toList (Set.fromList (map importModule (filter expands imports))))
  pure $
    resolveScope
      Known
        { knownFixities = \m -> Map.findWithDefault Nothing m table,
          knownChildren = \m -> Map.findWithDefault Map.empty m kept,
          knownExportNames = \m -> Map.findWithDefault Nothing m names
        }
      hsModule
  where
    expands i = case importNames i of
      Nothing -> False
      Just (_, items) -> any isAll items
    isAll = \case
      ImportedAll _ -> True
      _ -> False

-- | Everything a resolver consults, and the way back into it.
--
-- None of it changes from one module to the next, which is why
-- 'newResolverVia' builds it once and hands it over whole. What comes back
-- out of that is the 'Resolver'; this is what is behind it.
data Workings = Workings
  { -- | Which readings to try, in the order given.
    wkRoutes :: [Route],
    -- | Where to remember answers between runs.
    wkCache :: Maybe Cache,
    -- | The modules of the project's own packages, which are read straight
    -- from disk rather than out of an archive.
    wkLocal :: Map Text FilePath,
    -- | Which package holds each module, and the tarball to find it in; the
    -- package is the cache key, which carries the hash the tarball was
    -- verified against.
    wkIndex :: Map Text (Text, FilePath),
    -- | What to file an answer read out of each module's interface under.
    wkInterfaces :: Map Text (Text, FilePath),
    -- | A module's interface, read at most once a run.
    wkInterfaceOf :: Text -> IO (Maybe Interface),
    -- | How to reach another module. Tied back on itself by
    -- 'newResolverVia', so that the memo it keeps covers the recursive
    -- calls too.
    wkReach :: Set Text -> Text -> IO (Maybe (Map OpName Fixity)),
    -- | How to reach another module for what its names carry with them,
    -- tied back the same way and against a visiting set of its own.
    wkReachChildren :: Set Text -> Text -> IO (Map OpName (Set OpName)),
    -- | How to reach another module for what its export list names, tied
    -- back the same way again.
    wkReachExports :: Set Text -> Text -> IO (Maybe (Set OpName)),
    -- | What the package a module belongs to puts in force. A module that
    -- leans on its package's @default-extensions@ does not parse without
    -- them, and one that does not parse cannot be read for anything.
    wkExtensionsOf :: Text -> IO [Extension]
  }

-- | Where a module's fixities come from, in order of cost.
resolveModule ::
  -- | Where to look, and how to get back to the resolver.
  Workings ->
  -- | Modules currently being resolved further up the call chain.
  --
  -- Only passed through, so that a chase started here carries where it came
  -- from. What is done about a module already in it belongs to
  -- 'newResolverVia', which decides it before anything is remembered.
  Set Text ->
  -- | The module to resolve.
  Text ->
  -- | Its operator fixities, or 'Nothing' if they could not be
  -- established.
  IO (Maybe (Map OpName Fixity))
resolveModule
  Workings
    { wkRoutes,
      wkCache,
      wkLocal,
      wkIndex,
      wkInterfaces,
      wkInterfaceOf,
      wkReach,
      wkReachChildren,
      wkExtensionsOf
    }
  visiting
  modName
    | Just builtin <- Map.lookup modName builtinFixities = pure (Just builtin)
    | Just path <- Map.lookup modName wkLocal =
        readFileText path >>= \case
          Nothing -> pure Nothing
          Just source -> do
            extensions <- wkExtensionsOf modName
            fromText
              extensions
              (wkReach visiting')
              (wkReachChildren visiting')
              visiting'
              source
              modName
    | otherwise = answered <$> firstAnswer (map taking wkRoutes)
    where
      visiting' = Set.insert modName visiting

      taking = \case
        FromInterface -> viaInterface
        FromSource -> viaArchive

      firstAnswer [] = pure Unreadable
      firstAnswer (route : rest) =
        route >>= \case
          Just (Declares fixities) -> pure (Declares fixities)
          _ -> firstAnswer rest

      viaInterface = case Map.lookup modName wkInterfaces of
        Nothing -> pure Nothing
        Just (key, _) ->
          cachedFor key >>= \case
            Just remembered -> pure (Just remembered)
            Nothing -> do
              established <- fromInterface wkInterfaceOf modName
              storeFor key established
              pure (Just established)

      viaArchive = case Map.lookup modName wkIndex of
        Nothing -> pure Nothing
        Just (package, tarball) ->
          cachedFor package >>= \case
            Just remembered -> pure (Just remembered)
            Nothing -> do
              extensions <- wkExtensionsOf modName
              fromSource
                extensions
                (wkReach visiting')
                (wkReachChildren visiting')
                visiting'
                tarball
                modName
                >>= \case
                  NoArchive -> pure Nothing
                  FromArchive established -> do
                    storeFor package established
                    pure (Just established)

      answered = \case
        Declares fixities -> Just fixities
        Unreadable -> byHand modName
      byHand = (`Map.lookup` byHandFixities)
      cachedFor package = case wkCache of
        Nothing -> pure Nothing
        Just c -> cachedFixities c package modName
      storeFor package fixities = case wkCache of
        Nothing -> pure ()
        Just c -> storeFixities c package modName fixities

-- | Which package and tarball holds each module.
--
-- The module list of a package is itself cached: it comes from a @.cabal@
-- file inside an archive, and reading seventy of those is the bulk of what
-- starting up costs.
--
-- Where two packages expose the same module the first is kept. A plan that
-- builds cannot contain such a pair for any module the project imports, so
-- the choice only ever falls on a module nothing will ask about.
buildModuleIndex ::
  -- | Where to remember each package's module list, if anywhere.
  Maybe Cache ->
  -- | What the compiler says is installed. Empty if @ghc-pkg@ could not be
  -- run, in which case every package falls back to its @.cabal@ file.
  [InstalledPackage] ->
  -- | Every package that might have a tarball, and where it would be.
  [(PlanPackage, FilePath)] ->
  -- | For each module, the package that exposes it (as a cache key) and
  -- the tarball holding its source.
  IO (Map Text (Text, FilePath))
buildModuleIndex cache installed tarballs =
  Map.fromListWith (\_ first' -> first') . concat <$> traverse one tarballs
  where
    byNameVersion =
      Map.fromList [((ipName i, ipVersion i), ipModules i) | i <- installed]
    one (p, tarball) = do
      let key = cacheKey p
      let exposed = Map.lookup (ppName p, ppVersion p) byNameVersion
      held <- fromCabalFile cache key tarball p
      let modules = case (exposed, held) of
            (Nothing, Nothing) -> []
            (a, b) -> concat (catMaybes [a, b])
      pure [(m, (key, tarball)) | m <- modules]

-- | Where each installed module's compiled interface is.
--
-- Filed under the directory it was found in rather than under the package's
-- name and version, because those do not say which build: the same version
-- compiled with different flags can declare different fixities, and under
-- Nix a different build is a different directory.
interfaceIndex :: [InstalledPackage] -> Map Text (Text, FilePath)
interfaceIndex installed =
  Map.fromListWith
    (\_ first' -> first')
    [ (m, (key, dir </> T.unpack (T.replace "." "/" m) <> ".hi"))
    | i <- installed,
      dir <- ipImportDirs i,
      -- Bound out here so that the directory is hashed once rather than
      -- once for each of the modules found in it.
      let key = keyFor dir,
      m <- ipModules i
    ]
  where
    keyFor dir =
      "interface-"
        <> T.take 24 (T.decodeUtf8Lenient (B16.encode (SHA256.hash (T.encodeUtf8 (T.pack dir)))))

-- | Present a fixity map as an 'Interface'.
asInterface :: Map OpName Fixity -> Interface
asInterface fixities =
  Interface
    { interfaceDeclares = fixities,
      interfacePassedOn = [],
      interfaceChildren = Map.empty
    }

-- | The fixities a compiled interface reports, and those it passes on.
fromInterface ::
  -- | A module's interface, if it has one
  (Text -> IO (Maybe Interface)) ->
  -- | The module to read
  Text ->
  IO Established
fromInterface interfaceOf modName =
  interfaceOf modName >>= \case
    Nothing -> pure Unreadable
    Just iface -> do
      declarers <- traverse asked (distinct (map fst (interfacePassedOn iface)))
      pure $ case traverse snd declarers of
        Nothing -> Unreadable
        Just _ ->
          Declares . Map.union (interfaceDeclares iface) . Map.fromList $
            [ (op, fixity)
            | (m, op) <- interfacePassedOn iface,
              Just (Just declarer) <- [lookup m declarers],
              Just fixity <- [Map.lookup op (interfaceDeclares declarer)]
            ]
  where
    asked m = do
      interface <- interfaceOf m
      pure (m, interface)
    distinct = Map.keys . Map.fromList . map (,())

-- | A package's module list from the @.cabal@ file in its tarball.
fromCabalFile ::
  -- | Where to remember the answer, if anywhere.
  Maybe Cache ->
  -- | What to file it under. Carries the hash the tarball was verified
  -- against, so a changed tarball misses rather than matching stale data.
  Text ->
  -- | The tarball to read the @.cabal@ file out of.
  FilePath ->
  -- | The package it belongs to, consulted for the hash to verify against.
  PlanPackage ->
  -- | The modules it exposes, or 'Nothing' if the tarball is absent, fails
  -- verification, or holds no @.cabal@ file.
  IO (Maybe [Text])
fromCabalFile cache key tarball p = do
  remembered <- case cache of
    Nothing -> pure Nothing
    Just c -> cachedModules c key
  case remembered of
    -- A cached entry was written after the tarball was verified, and the
    -- key it is filed under contains the hash it was verified against, so a
    -- changed tarball simply misses rather than matching the wrong data.
    Just ms -> pure (Just ms)
    Nothing ->
      verified p tarball >>= \case
        False -> pure Nothing
        True ->
          packageModules tarball >>= \case
            Nothing -> pure Nothing
            Just ms -> do
              case cache of
                Nothing -> pure ()
                Just c -> storeModules c key ms
              pure (Just ms)

-- | How a package's cached answers are filed.
--
-- The expected hash is part of the key, so everything derived from a
-- tarball is bound to the exact bytes it was derived from. A package with
-- no hash in the plan is keyed by name and version alone.
cacheKey :: PlanPackage -> Text
cacheKey p =
  ppName p <> "-" <> ppVersion p <> maybe "" (("-" <>) . T.take 16) (sourceHashOf p)

-- | Does the tarball hash to what the plan says it should?
--
-- Hashing a few megabytes is not free, which is why it happens only on a
-- cache miss: once per package version per machine.
verified :: PlanPackage -> FilePath -> IO Bool
verified p tarball = case sourceHashOf p of
  Nothing -> pure True
  Just expected ->
    quietly False $ do
      actual <- sha256OfFile tarball
      pure (actual == T.toLower expected)

-- | The SHA-256 of a file, as lower-case hex.
sha256OfFile :: FilePath -> IO Text
sha256OfFile path = do
  bytes <- BL.readFile path
  pure (T.decodeUtf8Lenient (B16.encode (SHA256.hashlazy bytes)))

-- | Read a module's fixities out of a tarball, following re-exports.
fromSource ::
  -- | What the module's package puts in force, before its own pragmas.
  [Extension] ->
  -- | How to reach another module, for chasing re-exports. This is
  -- 'resolveModule' tied back on itself, with the visiting set already
  -- extended.
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  -- | How to reach another module for what its names carry with them.
  (Text -> IO (Map OpName (Set OpName))) ->
  -- | Modules currently being resolved, passed through so that a
  -- re-export chain cannot loop.
  Set Text ->
  -- | The tarball holding this module's source.
  FilePath ->
  -- | The module to read.
  Text ->
  -- | What it declares, including what it only passes on, and whether that
  -- is worth remembering.
  IO Reading
fromSource extensions reach reachChildren visiting tarball modName =
  doesFileExist tarball >>= \case
    False -> pure NoArchive
    True ->
      readModule tarball modName >>= \case
        Nothing -> pure (FromArchive Unreadable)
        Just source ->
          FromArchive . maybe Unreadable Declares
            <$> fromText extensions reach reachChildren visiting source modName

-- | What came of looking for a module in an archive.
data Reading
  = -- | The archive was there, and this is what reading it established.
    FromArchive Established
  | -- | There was no archive to open. That is a fact about this machine and
    -- not about the module—the plan can stay exactly as it is while
    -- somebody downloads the sources—so it is never remembered.
    NoArchive

-- | The fixities a module's text declares and passes on.
fromText ::
  -- | What the module's package puts in force, before its own pragmas.
  [Extension] ->
  -- | How to reach another module, for chasing re-exports. This is
  -- 'resolveModule' tied back on itself, with the visiting set already
  -- extended.
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  -- | How to reach another module for what its names carry with them.
  (Text -> IO (Map OpName (Set OpName))) ->
  -- | Modules currently being resolved, passed through so that a
  -- re-export chain cannot loop.
  Set Text ->
  -- | The module's source.
  Text ->
  -- | Its name.
  Text ->
  IO (Maybe (Map OpName Fixity))
fromText extensions reach reachChildren visiting source modName =
  case traverse parsed =<< configurations of
    Nothing -> pure Nothing
    Just modules ->
      agreeing <$> traverse (withReexports reach reachChildren visiting modName) modules
  where
    configurations = either (const Nothing) Just (branchLeaves source)
    parsed leaf =
      fmap pmModule
        . either (const Nothing) Just
        $ parseModule (configFor extensions leaf) (T.unpack modName) leaf

-- | One answer from every configuration that could be read, if they agree.
--
-- A module may declare a fixity in one configuration and a different one in
-- another. Which of them holds depends on how the module is compiled, which
-- is not ours to decide, so disagreement is not an answer. Agreement across
-- the ones we could read is one, and a stronger one than the blanked text
-- could give: it is a fact about the module rather than about a reading.
--
-- A configuration whose imports could not be resolved is passed over rather
-- than counted against the rest, because almost every one of those is a
-- branch meant for somewhere else. @System.IO.CodePage@ imports
-- @System.Win32.CodePage@ under @#ifdef WINDOWS@, and no plan solved on
-- Linux has Win32 anywhere in it. Refusing the whole module over a branch
-- that will never be compiled here would be letting a fact about this
-- machine stand as a fact about the module.
--
-- Every configuration unresolvable is still no answer. There is nothing
-- left to agree, and saying the module declares nothing would be a guess
-- rather than the silence it deserves.
agreeing :: [Maybe (Map OpName Fixity)] -> Maybe (Map OpName Fixity)
agreeing answers = case catMaybes answers of
  [] -> Nothing
  readable -> foldM together Map.empty readable
  where
    together settled found
      | and (Map.intersectionWith (==) settled found) = Just (Map.union settled found)
      | otherwise = Nothing

-- | Where each module of the project's own packages lives.
--
-- A local package is a directory rather than an archive, so its modules are
-- found by putting the @hs-source-dirs@ of its @.cabal@ file together with
-- the module names it exposes. Nothing is unpacked and nothing is cached:
-- these are the files being worked on.
localModules :: BuildPlan -> IO (Map Text FilePath)
localModules plan =
  Map.unions <$> traverse forPackage [d | LocalPackage d <- map ppSource (bpPackages plan)]
  where
    forPackage dir = quietly Map.empty $ do
      entries <- listDirectory dir
      case filter (".cabal" `isSuffixOf`) entries of
        [] -> pure Map.empty
        (cabalFile : _) -> do
          contents <- readFileText (dir </> cabalFile)
          case contents of
            Nothing -> pure Map.empty
            Just text ->
              Map.fromList . concat
                <$> traverse (locate dir (sourceDirs text)) (containedModules text)

    -- A package may list several source directories and the @.cabal@ file
    -- does not say which one holds which module, so they are tried in turn
    -- and the first that has the file wins.
    locate dir dirs m = do
      found <- filterM doesFileExist [dir </> T.unpack d </> modulePath m | d <- dirs]
      pure [(m, path) | path <- take 1 found]

    modulePath m = T.unpack (T.replace "." "/" m) <> ".hs"

-- | Read a file, if it is there and is text.
readFileText :: FilePath -> IO (Maybe Text)
readFileText path = quietly Nothing $ do
  there <- doesFileExist path
  if there
    then Just . T.decodeUtf8Lenient <$> BS.readFile path
    else pure Nothing

-- | What a module passes on, as well as what it declares.
--
-- A module that exports an operator it did not declare carries no fixity of
-- its own for it, so the declaration is chased through the export list into
-- whichever module the name came from.
withReexports ::
  -- | How to reach another module, for names this one only passes on.
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  -- | How to reach another module for what its names carry with them,
  -- which is what a @T(..)@ this module hands on amounts to.
  (Text -> IO (Map OpName (Set OpName))) ->
  -- | Modules currently being resolved. A candidate already in here is
  -- skipped rather than followed.
  Set Text ->
  -- | The name this module was looked up under, used to recognise a
  -- @module M@ export that refers to the module itself.
  Text ->
  -- | The module, already parsed.
  HsModule GhcPs ->
  -- | What it declares together with what it re-exports, or 'Nothing' if a
  -- module it passes names on from could not be read.
  IO (Maybe (Map OpName Fixity))
withReexports reach reachChildren visiting modName hsModule =
  case moduleExports hsModule of
    Nothing -> pure (Just own)
    Just items -> do
      carried <- carriedNames reachChildren hsModule items
      let wanted = wantedNames items <> fromCarried carried
      visible <-
        if null wanted
          then pure (Just [])
          else
            sequence
              <$> traverse
                (\i -> fmap ((,) i) <$> fromModule (importModule i))
                (moduleImports hsModule)
      wholeModules <- sequence <$> traverse fromModule (wantedModules modName hsModule items)
      pure $ do
        seen <- visible
        whole <- wholeModules
        let passedOn =
              Map.fromList
                [ (op, fixity)
                | (qualifier, op) <- wanted,
                  fixity <- take 1 (from qualifier op seen)
                ]
        pure (Map.unions (own : passedOn : whole))
  where
    own = declaredFixities hsModule
    defined = declaredNames hsModule
    wantedNames items =
      [(qualifier, op) | ExportName qualifier op <- items, not (Set.member op defined)]
        <> [(qualifier, op) | ExportAll qualifier op <- items, not (Set.member op defined)]
    fromCarried carried =
      [ (qualifier, op)
      | ((qualifier, _), Just ops) <- carried,
        op <- Set.toList ops,
        not (Set.member op defined)
      ]
    from qualifier op seen =
      [ fixity
      | (i, exported) <- seen,
        canSupply qualifier op i,
        Just fixity <- [Map.lookup op exported]
      ]
    fromModule m
      | m `Set.member` visiting = pure (Just Map.empty)
      | otherwise = reach m

-- | What each name a module's export list hands on carries with it.
childrenWithReexports ::
  -- | How to reach another module for what its names carry
  (Text -> IO (Map OpName (Set OpName))) ->
  -- | The name this module was looked up under
  Text ->
  -- | The module, already parsed
  HsModule GhcPs ->
  IO (Map OpName (Set OpName))
childrenWithReexports reachChildren modName hsModule =
  case moduleExports hsModule of
    Nothing -> pure (moduleChildren hsModule)
    Just items -> do
      carried <- carriedNames reachChildren hsModule items
      wholes <- traverse reachChildren (wantedModules modName hsModule items)
      pure . Map.unionsWith Set.union $
        moduleChildren hsModule
          : Map.fromListWith Set.union [(parent, ops) | ((_, parent), Just ops) <- carried]
          : wholes

-- | The operators a module's export list names, following what it hands on.
--
-- 'exportedOperators' answers for a list that names everything outright.
-- Where the list hands a whole module on, or a @T(..)@ for a type declared
-- elsewhere, the answer is in another module and this goes and gets it.
--
-- 'Nothing' where any part of the list stays beyond us, since a set that
-- leaves names out would clear a module of carrying an operator it may
-- well carry. Everything or nothing: this answer is only ever used to rule
-- a module out.
exportNamesWithReexports ::
  -- | How to reach another module for what its export list names
  (Text -> IO (Maybe (Set OpName))) ->
  -- | How to reach another module for what its names carry
  (Text -> IO (Map OpName (Set OpName))) ->
  -- | The name this module was looked up under
  Text ->
  -- | The module, already parsed
  HsModule GhcPs ->
  IO (Maybe (Set OpName))
exportNamesWithReexports reachNames reachChildren modName hsModule =
  case moduleExports hsModule of
    Nothing -> pure (Just (Map.keysSet (declaredFixities hsModule)))
    Just items -> do
      carried <- carriedNames reachChildren hsModule items
      wholes <- traverse reachNames (wantedModules modName hsModule items)
      pure $ do
        fromWholes <- sequence wholes
        fromCarried <-
          traverse (\((_, parent), kids) -> Set.insert parent <$> kids) carried
        pure (Set.unions (named items : declaredHere items : fromCarried <> fromWholes))
  where
    declared = declaredChildren hsModule
    named items = Set.fromList [op | ExportName _ op <- items]
    declaredHere items =
      Set.unions
        [ Set.insert parent kids
        | ExportAll _ parent <- items,
          Just kids <- [Map.lookup parent declared]
        ]

-- | What the types a module hands on but does not declare carry with them,
-- asked of the modules they could have come from.
carriedNames ::
  (Text -> IO (Map OpName (Set OpName))) ->
  HsModule GhcPs ->
  [ExportItem] ->
  -- | For each handed-on name, what it carries, or 'Nothing' where no
  -- module that could have supplied it had anything to say about it.
  IO [((Maybe Text, OpName), Maybe (Set OpName))]
carriedNames reachChildren hsModule items =
  traverse (\(qualifier, parent) -> ((qualifier, parent),) <$> carriedBy qualifier parent) handedOn
  where
    declared = declaredChildren hsModule
    imports = moduleImports hsModule
    handedOn =
      [ (qualifier, parent)
      | ExportAll qualifier parent <- items,
        not (Map.member parent declared)
      ]
    carriedBy qualifier parent = do
      answers <- traverse (reachChildren . importModule) (filter (canSupply qualifier parent) imports)
      pure $ case mapMaybe (Map.lookup parent) answers of
        [] -> Nothing
        kids -> Just (Set.unions kids)

-- | Could this import have supplied a name an export list hands on?
canSupply :: Maybe Text -> OpName -> Import -> Bool
canSupply qualifier op i =
  reaches && case importNames i of
    Nothing -> True
    Just (True, hidden) -> not (surelyNames Map.empty op hidden)
    Just (False, shown) -> mightBring Map.empty op shown
  where
    reaches = case qualifier of
      Nothing -> not (importQualified i)
      Just q -> importAlias i == q

-- | The modules a @module M@ export hands on whole, by their own names.
wantedModules :: Text -> HsModule GhcPs -> [ExportItem] -> [Text]
wantedModules modName hsModule items =
  Set.toList . Set.fromList $
    concat [under m | ExportModule m <- items, not (isSelf m)]
  where
    under m = case [importModule i | i <- imports, importAlias i == m] of
      [] -> [m]
      aliased -> aliased
    imports = moduleImports hsModule
    isSelf m = Just m == moduleName hsModule || m == modName

-- | What to parse a module with: what its package puts in force, and then
-- whatever its own pragmas say about that.
configFor :: [Extension] -> Text -> ParserConfig
configFor extensions source = parserConfigFor (effectiveExtensions extensions source)

-- | Find a module inside a tarball and decode it.
readModule :: FilePath -> Text -> IO (Maybe Text)
readModule tarball modName = quietly Nothing $ do
  bytes <- BL.readFile tarball
  let dirs = maybe [] sourceDirs (findCabalFile (Tar.read (GZip.decompress bytes)))
  pure (pick dirs (matching (Tar.read (GZip.decompress bytes))))
  where
    suffix = "/" <> T.unpack (T.replace "." "/" modName) <> ".hs"
    matching = go []
      where
        go found = \case
          Tar.Next entry rest
            | suffix `isSuffixOf` Tar.entryPath entry,
              Tar.NormalFile content _ <- Tar.entryContent entry ->
                go ((Tar.entryPath entry, decode content) : found) rest
            | otherwise -> go found rest
          _ -> reverse found
    pick dirs found = snd <$> listToMaybe (under dirs found <> found)
    under dirs found = [e | d <- dirs, e <- found, inDir d (fst e)]
    inDir d path
      | d == "." = takeWhile (/= '/') path <> suffix == path
      | otherwise = ("/" <> T.unpack d <> suffix) `isSuffixOf` path
    decode = T.decodeUtf8Lenient . BL.toStrict
