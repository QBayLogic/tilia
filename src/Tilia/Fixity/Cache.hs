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
    Established (..),
    openCache,
    cachedModules,
    storeModules,
    cachedFixities,
    storeFixities,
    cachedInstalled,
    storeInstalled,
  )
where

import Control.Monad (join)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory
  ( XdgDirectory (XdgCache),
    createDirectoryIfMissing,
    doesFileExist,
    getModificationTime,
    getXdgDirectory,
    renameFile,
  )
import System.FilePath ((</>))
import Tilia.Fixity
import Tilia.Fixity.PackageDb (Installed (..), InstalledPackage (..))
import Tilia.Utils (quietly)

-- | Where cached answers are kept together with a token unique to this
-- build plan.
data Cache = Cache FilePath PlanToken

-- | A token that is unique to this plan. It is needed in order to be able
-- to cache the expensive class of lookup failures that are related to
-- chasing module re-export chains. Rather than track which packages each
-- failure leaned on, all of them are tied to the plan as a whole: anything
-- that could turn a failure into an answer changes the plan, and failures
-- are few enough that re-deriving them when it does costs little.
newtype PlanToken = PlanToken Text
  deriving (Eq, Show)

-- | What reading a module established about its operators.
data Established
  = -- | It was read, and declares these.
    Declares (Map OpName Fixity)
  | -- | It could not be read. The expensive answer of the two, because
    -- reaching it means exhausting every way of reading the module, which
    -- is why it is kept rather than worked out again on every run.
    Unreadable
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
      ("read" : entries) -> Just (Declares (Map.fromList (mapMaybe parseEntry entries)))
      [unread] | unread == "unread\t" <> token -> Just Unreadable
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
      Unreadable -> T.unlines ["unread\t" <> token]
      Declares fixities ->
        T.unlines ("read" : map renderEntry (Map.toList fixities))
  where
    Cache _ (PlanToken token) = cache

----------------------------------------------------------------------------
-- The package database

-- | What the compiler could see when last asked, if it can still see it.
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
      ["pkg", name, version, modules] ->
        Just InstalledPackage
               {ipName = name, ipVersion = version, ipModules = T.words modules}
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
          <> [ T.intercalate "\t" ["pkg", ipName p, ipVersion p, T.unwords (ipModules p)]
             | p <- installedPackages found
             ]
  where
    stamped path = do
      stamp <- T.pack . show <$> getModificationTime path
      pure (path, stamp)

----------------------------------------------------------------------------
-- Entries

renderEntry :: (OpName, Fixity) -> Text
renderEntry (OpName op, Fixity direction precedence) =
  T.intercalate "\t" [op, renderDirection direction, T.pack (show precedence)]
  where
    renderDirection = \case
      LeftAssoc -> "l"
      RightAssoc -> "r"
      NoAssoc -> "n"

parseEntry :: Text -> Maybe (OpName, Fixity)
parseEntry line = case T.splitOn "\t" line of
  [op, direction, precedence] -> do
    d <- parseDirection direction
    p <- readPrecedence precedence
    pure (OpName op, Fixity d p)
  _ -> Nothing
  where
    parseDirection = \case
      "l" -> Just LeftAssoc
      "r" -> Just RightAssoc
      "n" -> Just NoAssoc
      _ -> Nothing
    readPrecedence t = case decimal' t of
      Just n | n >= 0, n <= 9 -> Just n
      _ -> Nothing

-- | A non-negative integer, or nothing.
decimal' :: Text -> Maybe Int
decimal' t
  | T.null t = Nothing
  | T.all (`elem` ['0' .. '9']) t = Just (T.foldl' step 0 t)
  | otherwise = Nothing
  where
    step acc c = acc * 10 + (fromEnum c - fromEnum '0')

----------------------------------------------------------------------------
-- Paths and files

packageDir :: Cache -> Text -> FilePath
packageDir (Cache root _) package = root </> "fixities" </> T.unpack package

installedPath :: Cache -> FilePath
installedPath (Cache root _) = root </> "installed"

modulesPath :: Cache -> Text -> FilePath
modulesPath (Cache root _) package = root </> "modules" </> T.unpack package

fixitiesPath :: Cache -> Text -> Text -> FilePath
fixitiesPath cache package modName =
  packageDir cache package </> T.unpack modName

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
