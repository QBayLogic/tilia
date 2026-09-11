{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Remembering what was read out of a package, between runs.
--
-- Reading a dependency means decompressing an archive and parsing a module,
-- which costs tens of milliseconds. Doing it once is fine. Doing it again
-- for every file of a project, on every save, is not—and that is the shape
-- of the work when a formatter is driven from an editor.
module Tilia.Fixity.Cache
  ( Cache,
    PlanToken (..),
    openCache,
    cachedModules,
    storeModules,
    cachedFixities,
    storeFixities,
    cachedExportNames,
    storeExportNames,
    cachedChildren,
    storeChildren,
    cachedInstalled,
    storeInstalled,
    cachedFutileSolve,
    storeFutileSolve,
    cachedFutileFetch,
    storeFutileFetch,
  )
where

import Control.Monad (join)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T
import System.Directory
  ( XdgDirectory (XdgCache),
    createDirectoryIfMissing,
    doesFileExist,
    getModificationTime,
    getXdgDirectory,
    renameFile,
  )
import System.FilePath (takeDirectory, (</>))
import Tilia.Fixity
import Tilia.Fixity.PackageDb (Installed (..), InstalledPackage (..))
import Tilia.Utils (quietly)

-- | Where cached answers are kept together with a token unique to this
-- build plan, read in this environment.
data Cache = Cache FilePath PlanToken

-- | A token that is unique to this plan, read in this environment. It is
-- needed in order to be able to cache the expensive class of lookup
-- failures that are related to chasing module re-export chains. Rather than
-- track what each failure leaned on, all of them are tied to the pair as a
-- whole: anything that could turn a failure into an answer changes one or
-- the other, and failures are few enough that re-deriving them when it does
-- costs little.
--
-- The environment belongs in it because a module the plan names is
-- unreadable where the compiler cannot be asked about its package and
-- readable where it can, and that is settled outside the project. See
-- 'Tilia.Fixity.PackageDb.compilerIdentity'.
newtype PlanToken = PlanToken Text
  deriving (Eq, Show)

-- | Bumped whenever what is written changes shape, so that entries from an
-- older Tilia are ignored rather than misread.
formatVersion :: FilePath
formatVersion = "v1"

-- | Open, creating the directory if need be.
--
-- 'Nothing' if there is nowhere to write, in which case everything still
-- works and is merely slower.
--
-- The token is what a failure written through this cache is tied to, and
-- what one read back out of it has to match. See 'PlanToken'.
openCache :: PlanToken -> IO (Maybe Cache)
openCache token = quietly Nothing $ do
  root <- (</> formatVersion) <$> getXdgDirectory XdgCache "tilia"
  createDirectoryIfMissing True (root </> "modules")
  createDirectoryIfMissing True (root </> "fixities")
  createDirectoryIfMissing True (root </> "installed")
  pure (Just (Cache root token))

-- | The modules a package exposes, if that was worked out before.
cachedModules :: Cache -> Text -> IO (Maybe [Text])
cachedModules cache package =
  readIfPresent (modulesPath cache package) $
    filter (not . T.null) . T.lines

-- | Remember what a package exposes.
storeModules :: Cache -> Text -> [Text] -> IO ()
storeModules cache package =
  writeAtomically (modulesPath cache package) . T.unlines

-- | What was established about a module before, if anything was.
--
-- An 'Unreadable' answer is offered back only under the 'PlanToken' it was
-- written under.
cachedFixities ::
  -- | Where to look
  Cache ->
  -- | The package the module belongs to. Opaque here: whatever the caller
  -- uses to tell one package from another is what an answer is filed under,
  -- and answers filed under different keys never meet.
  Text ->
  -- | The module, by its full dotted name
  Text ->
  -- | What was established, or 'Nothing' if nothing was
  IO (Maybe Established)
cachedFixities cache@(Cache _ (PlanToken token)) package modName =
  fmap join . readIfPresent (fixitiesPath cache package modName) $ \contents ->
    case T.lines contents of
      ("read" : entries) -> Declares . Map.fromList <$> traverse parseEntry entries
      [unread] | Just rest <- T.stripPrefix ("unread\t" <> token) unread ->
        case T.uncons rest of
          Nothing -> Just (Unreadable Nothing)
          Just ('\t', below) | not (T.null below) -> Just (Unreadable (Just below))
          _ -> Nothing
      _ -> Nothing

-- | Remember what reading a module established.
storeFixities ::
  -- | Where to write
  Cache ->
  -- | The package the module belongs to, as 'cachedFixities' takes it
  Text ->
  -- | The module, by its full dotted name
  Text ->
  -- | What was established about it
  Established ->
  IO ()
storeFixities cache package modName answer = do
  quietly () (createDirectoryIfMissing True (packageDir cache package))
  writeAtomically (fixitiesPath cache package modName) $
    case answer of
      Unreadable below ->
        T.unlines ["unread\t" <> token <> foldMap ("\t" <>) below]
      Declares fixities ->
        T.unlines ("read" : map renderEntry (Map.toList fixities))
  where
    Cache _ (PlanToken token) = cache

----------------------------------------------------------------------------
-- Export names

-- | What a module's export list was found to say, if it was ever read.
--
-- Wanted for the modules whose fixities could not be established, and asked
-- exactly then: a warm cache answers those from 'cachedFixities' without
-- opening the archive at all, and without this the archive would be opened
-- anyway to ask this instead.
cachedExportNames ::
  -- | Where to look
  Cache ->
  -- | The package the module belongs to, as 'cachedFixities' takes it
  Text ->
  -- | The module, by its full dotted name
  Text ->
  -- | What its export list said, or 'Nothing' if it was never read
  IO (Maybe Exported)
cachedExportNames cache package modName =
  fmap join . readIfPresent (exportsPath cache package modName) $ \contents ->
    case T.lines contents of
      ("names" : entries) -> Just (Exports (Set.fromList (map OpName entries)))
      ["untellable"] -> Just Untellable
      _ -> Nothing

-- | Remember what a module's export list said.
storeExportNames ::
  -- | Where to write
  Cache ->
  -- | The package the module belongs to, as 'cachedFixities' takes it
  Text ->
  -- | The module, by its full dotted name
  Text ->
  -- | What its export list said
  Exported ->
  IO ()
storeExportNames cache package modName answer = do
  quietly () (createDirectoryIfMissing True (exportsDir cache package))
  writeAtomically (exportsPath cache package modName) $
    case answer of
      Untellable -> T.unlines ["untellable"]
      Exports names -> T.unlines ("names" : [op | OpName op <- Set.toAscList names])

----------------------------------------------------------------------------
-- What a name carries with it

-- | What a module keeps under each of its names, if it was ever read for
-- it.
--
-- Wanted wherever an import list writes @T(..)@, and got at by reading the
-- module's interface or its source—which on a warm cache is work that
-- would otherwise not be done at all.
cachedChildren ::
  -- | Where to look
  Cache ->
  -- | The package the module belongs to, as 'cachedFixities' takes it
  Text ->
  -- | The module, by its full dotted name
  Text ->
  -- | What it keeps under each name, or 'Nothing' if it was never read
  IO (Maybe (Map OpName (Set OpName)))
cachedChildren cache package modName =
  fmap join . readIfPresent (childrenPath cache package modName) $ \contents ->
    case T.lines contents of
      ("children" : entries) -> Just (Map.fromList (mapMaybe childEntry entries))
      _ -> Nothing
  where
    childEntry line = case T.splitOn "\t" line of
      (parent : kids) -> Just (OpName parent, Set.fromList (map OpName kids))
      [] -> Nothing

-- | Remember what a module keeps under each of its names.
storeChildren ::
  -- | Where to write
  Cache ->
  -- | The package the module belongs to, as 'cachedFixities' takes it
  Text ->
  -- | The module, by its full dotted name
  Text ->
  -- | What it keeps under each name
  Map OpName (Set OpName) ->
  IO ()
storeChildren cache package modName children = do
  quietly () (createDirectoryIfMissing True (childrenDir cache package))
  writeAtomically (childrenPath cache package modName) $
    T.unlines ("children" : map entry (Map.toList children))
  where
    entry (OpName parent, kids) =
      T.intercalate "\t" (parent : [kid | OpName kid <- Set.toAscList kids])

----------------------------------------------------------------------------
-- The package database

-- | What the compiler could see when last asked, if it can still see it.
--
-- Whose answer this is, is settled by the path it was found at rather than
-- by anything written inside it. See 'installedPath'.
cachedInstalled :: Cache -> IO (Maybe [InstalledPackage])
cachedInstalled cache = quietly Nothing $ do
  readIfPresent (installedPath cache) T.lines >>= \case
    Nothing -> pure Nothing
    Just ls -> do
      let written =
            [(T.unpack path, stamp) | ["db", path, stamp] <- map fields ls]
      still <- traverse unchanged written
      pure $
        if not (null written) && and still
          then Just (mapMaybe installedFrom ls)
          else Nothing
  where
    unchanged (path, stamp) =
      quietly False ((== stamp) . T.pack . show <$> getModificationTime path)
    installedFrom l = case fields l of
      ("pkg" : name : version : modules : dirs) ->
        Just
          InstalledPackage
            { ipName = name,
              ipVersion = version,
              ipModules = T.words modules,
              ipImportDirs = map T.unpack dirs
            }
      _ -> Nothing
    fields = T.splitOn "\t"

-- | Remember what the compiler can see, stamped so that a later run can
-- tell whether it still does.
--
-- Nothing is written when there is no database to stamp: an answer nothing
-- can invalidate is worse than no answer, because it never stops being
-- given.
storeInstalled :: Cache -> Installed -> IO ()
storeInstalled cache found
  | null (installedDatabases found) = pure ()
  | otherwise = quietly () $ do
      stamps <- traverse stamped (installedDatabases found)
      writeAtomically (installedPath cache) . T.unlines $
        [T.intercalate "\t" ["db", T.pack path, stamp] | (path, stamp) <- stamps]
          <> [ T.intercalate "\t" $
                 ["pkg", ipName p, ipVersion p, T.unwords (ipModules p)]
                   <> map T.pack (ipImportDirs p)
             | p <- installedPackages found
             ]
  where
    stamped path = do
      stamp <- T.pack . show <$> getModificationTime path
      pure (path, stamp)

----------------------------------------------------------------------------
-- Solves that came to nothing

-- | Whether asking @cabal@ to solve this plan again has already been tried
-- and left the plan saying exactly what it said before.
cachedFutileSolve :: Cache -> IO Bool
cachedFutileSolve cache =
  quietly False (doesFileExist (futileSolvePath cache))

-- | Remember that solving again did not widen the plan.
storeFutileSolve :: Cache -> IO ()
storeFutileSolve cache = quietly () $ do
  createDirectoryIfMissing True (takeDirectory (futileSolvePath cache))
  writeAtomically (futileSolvePath cache) ""

-- | The packages an earlier run was still short of after asking @cabal@ to
-- fetch them, and which asking again will therefore not bring in.
cachedFutileFetch :: Cache -> IO [Text]
cachedFutileFetch cache =
  fromMaybe []
    <$> readIfPresent
      (futileFetchPath cache)
      (filter (not . T.null) . T.lines)

-- | Remember what fetching left missing.
storeFutileFetch :: Cache -> [Text] -> IO ()
storeFutileFetch cache packages = quietly () $ do
  createDirectoryIfMissing True (takeDirectory (futileFetchPath cache))
  writeAtomically (futileFetchPath cache) (T.unlines packages)

----------------------------------------------------------------------------
-- Entries

renderEntry :: ((Namespace, OpName), Fixity) -> Text
renderEntry ((namespace, OpName op), Fixity direction precedence) =
  T.intercalate
    "\t"
    [op, renderNamespace namespace, renderDirection direction, T.pack (show precedence)]
  where
    renderNamespace = \case
      InTypes -> "t"
      InTerms -> "v"
    renderDirection = \case
      LeftAssoc -> "l"
      RightAssoc -> "r"
      NoAssoc -> "n"

parseEntry :: Text -> Maybe ((Namespace, OpName), Fixity)
parseEntry line = case T.splitOn "\t" line of
  [op, namespace, direction, precedence] -> do
    n <- parseNamespace namespace
    d <- parseDirection direction
    p <- readPrecedence precedence
    pure ((n, OpName op), Fixity d p)
  _ -> Nothing
  where
    parseNamespace = \case
      "t" -> Just InTypes
      "v" -> Just InTerms
      _ -> Nothing
    parseDirection = \case
      "l" -> Just LeftAssoc
      "r" -> Just RightAssoc
      "n" -> Just NoAssoc
      _ -> Nothing
    readPrecedence t = case T.signed T.decimal t of
      Right (p, rest) | T.null rest -> Just p
      _ -> Nothing

----------------------------------------------------------------------------
-- Paths and files

packageDir :: Cache -> Text -> FilePath
packageDir (Cache root _) package = root </> "fixities" </> T.unpack package

-- | Under the token, as everything else here is filed under the key that
-- decides it.
--
-- One file shared by every token could only ever carry a check saying whose
-- it was, which answers \"is this mine?\" and never \"where is mine?\": two
-- environments formatting the same project would take turns discarding each
-- other's answer and asking @ghc-pkg@ again.
installedPath :: Cache -> FilePath
installedPath (Cache root (PlanToken token)) =
  root </> "installed" </> T.unpack token

futileSolvePath :: Cache -> FilePath
futileSolvePath (Cache root (PlanToken token)) =
  root </> "solves" </> T.unpack token

futileFetchPath :: Cache -> FilePath
futileFetchPath (Cache root (PlanToken token)) =
  root </> "fetches" </> T.unpack token

modulesPath :: Cache -> Text -> FilePath
modulesPath (Cache root _) package = root </> "modules" </> T.unpack package

fixitiesPath :: Cache -> Text -> Text -> FilePath
fixitiesPath cache package modName =
  packageDir cache package </> T.unpack modName

exportsDir :: Cache -> Text -> FilePath
exportsDir (Cache root _) package = root </> "exports" </> T.unpack package

exportsPath :: Cache -> Text -> Text -> FilePath
exportsPath cache package modName =
  exportsDir cache package </> T.unpack modName

childrenDir :: Cache -> Text -> FilePath
childrenDir (Cache root _) package = root </> "children" </> T.unpack package

childrenPath :: Cache -> Text -> Text -> FilePath
childrenPath cache package modName =
  childrenDir cache package </> T.unpack modName

readIfPresent :: FilePath -> (Text -> a) -> IO (Maybe a)
readIfPresent path parse = quietly Nothing $ do
  there <- doesFileExist path
  if there then Just . parse <$> T.readFile path else pure Nothing

-- | Write via a temporary file and a rename.
--
-- Two formatters may run at once—an editor saving while a pre-commit hook
-- runs—and a half-written entry read by the other would be worse than no
-- entry at all. A rename is atomic, so a reader sees the old file or the
-- new one and never a partial one.
--
-- If anything fails the write is abandoned. A stray temporary left in a
-- cache directory costs nothing; a wrong answer would cost a great deal.
writeAtomically :: FilePath -> Text -> IO ()
writeAtomically path contents = quietly () $ do
  let temporary = path <> ".tmp"
  T.writeFile temporary contents
  renameFile temporary path
