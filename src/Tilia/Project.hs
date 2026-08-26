{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Finding the project a file belongs to.
--
-- A formatter is usually handed a file, not a project. An editor may run it
-- with the working directory set to the file's own directory, or to the
-- editor's, or to wherever the user happened to start it. None of those is
-- reliably the root, and the root is what the build plan and the package
-- cache are found relative to.
module Tilia.Project
  ( ProjectRoot (..),
    findProjectRoot,
  )
where

import Data.List (isSuffixOf)
import System.Directory
  ( canonicalizePath,
    doesDirectoryExist,
    listDirectory,
  )
import System.FilePath (takeDirectory)
import Tilia.Utils (quietly)

-- | A project, and what marked it out.
data ProjectRoot = ProjectRoot
  { -- | The directory.
    prPath :: FilePath,
    -- | The file that identified it, for reporting.
    prMarker :: FilePath
  }
  deriving (Eq, Show)

-- | Walk upwards from a file or directory looking for a project.
--
-- The markers are tried in order of authority at each level before moving
-- up, so a package inside a multi-package repository resolves to the
-- repository rather than to itself:
--
--   * @cabal.project@ — names the whole build, and is what @cabal@ solves
--   * @stack.yaml@ — the same for Stack
--   * any @.cabal@ file — a single package with no project around it
--
-- Stops at the filesystem root and returns 'Nothing' rather than guessing.
-- Formatting a file that belongs to no project is a perfectly ordinary
-- thing to do; it simply cannot have its fixities resolved.
findProjectRoot :: FilePath -> IO (Maybe ProjectRoot)
findProjectRoot start = quietly Nothing $ do
  from <- startingDirectory
  climb from
  where
    startingDirectory = do
      absolute <- canonicalizePath start
      isDirectory <- doesDirectoryExist absolute
      pure (if isDirectory then absolute else takeDirectory absolute)

    climb directory =
      markerIn directory >>= \case
        Just marker -> pure (Just (ProjectRoot directory marker))
        Nothing ->
          let parent = takeDirectory directory
           in if parent == directory then pure Nothing else climb parent

    markerIn directory = quietly Nothing $ do
      entries <- listDirectory directory
      pure $ case filter (`elem` entries) ["cabal.project", "stack.yaml"] of
        (named : _) -> Just named
        [] -> case filter (".cabal" `isSuffixOf`) entries of
          (packageFile : _) -> Just packageFile
          [] -> Nothing

