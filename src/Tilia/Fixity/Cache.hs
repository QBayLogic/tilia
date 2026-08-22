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
    openCache,
    cachedModules,
    storeModules,
    cachedFixities,
    storeFixities,
  )
where

import Control.Exception (SomeException, try)
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
    getXdgDirectory,
    renameFile,
  )
import System.FilePath ((</>))
import Tilia.Fixity

-- | Where cached answers are kept.
newtype Cache = Cache FilePath

-- | Bumped whenever what is written changes shape, so that entries from an
-- older Tilia are ignored rather than misread.
formatVersion :: FilePath
formatVersion = "v1"

-- | Open, creating the directory if need be.
--
-- 'Nothing' if there is nowhere to write, in which case everything still
-- works and is merely slower.
openCache :: IO (Maybe Cache)
openCache = quietly Nothing $ do
  root <- (</> formatVersion) <$> getXdgDirectory XdgCache "tilia"
  createDirectoryIfMissing True (root </> "modules")
  createDirectoryIfMissing True (root </> "fixities")
  pure (Just (Cache root))

-- | The modules a package exposes, if that was worked out before.
cachedModules :: Cache -> Text -> IO (Maybe [Text])
cachedModules cache package =
  readIfPresent (modulesPath cache package) $
    filter (not . T.null) . T.lines

-- | Remember what a package exposes.
storeModules :: Cache -> Text -> [Text] -> IO ()
storeModules cache package =
  writeAtomically (modulesPath cache package) . T.unlines

-- | The fixities a module declares, if that was worked out before.
cachedFixities :: Cache -> Text -> Text -> IO (Maybe (Map OpName Fixity))
cachedFixities cache package modName =
  readIfPresent (fixitiesPath cache package modName) $
    Map.fromList . mapMaybe parseEntry . T.lines

-- | Remember what a module declares.
--
-- An empty map is worth storing: "this module declares no operators" is an
-- answer, and re-deriving it costs exactly as much as any other.
storeFixities :: Cache -> Text -> Text -> Map OpName Fixity -> IO ()
storeFixities cache package modName fixities = do
  quietly () (createDirectoryIfMissing True (packageDir cache package))
  writeAtomically (fixitiesPath cache package modName) $
    T.unlines (map renderEntry (Map.toList fixities))

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
packageDir (Cache root) package = root </> "fixities" </> T.unpack package

modulesPath :: Cache -> Text -> FilePath
modulesPath (Cache root) package = root </> "modules" </> T.unpack package

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

quietly :: a -> IO a -> IO a
quietly fallback action =
  try action >>= \case
    Left (_ :: SomeException) -> pure fallback
    Right a -> pure a
