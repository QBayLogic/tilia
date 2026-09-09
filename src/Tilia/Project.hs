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
    Marker (..),
    markerFile,
    findProjectRoot,
  )
where

import Data.List (isSuffixOf)
import Data.Maybe (listToMaybe)
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
    -- | What identified it.
    prMarker :: Marker
  }
  deriving (Eq, Show)

-- | What a project was recognised by.
--
-- The two are read differently—one names the packages of a build, the
-- other is a package—so which it was has to survive being found.
data Marker
  = -- | A @cabal.project@, which names the packages.
    ProjectFile
  | -- | A @.cabal@ file, by its name: a package with no project around it.
    PackageFile FilePath
  deriving (Eq, Show)

-- | The file a marker stands for, for reporting.
markerFile :: Marker -> FilePath
markerFile = \case
  ProjectFile -> "cabal.project"
  PackageFile named -> named

-- | Walk upwards from a file or directory looking for a project.
--
-- A @cabal.project@ anywhere above wins over a @.cabal@ file nearer to
-- hand, so that a package inside a multi-package repository resolves to the
-- repository rather than to itself. That is where @cabal@ solves the build
-- and writes the plan, and a run rooted at the package would go looking for
-- a plan that is one directory up.
--
-- Only what @cabal@ reads counts. A @stack.yaml@ is not a project here: it
-- would stop the climb at a directory @cabal@ cannot solve in, whereas
-- passing over it settles on a @.cabal@ that @cabal@ can, which is the
-- difference between formatting a stack project and refusing it.
--
-- Stops at the filesystem root and returns 'Nothing' rather than guessing.
-- Formatting a file that belongs to no project is a perfectly ordinary
-- thing to do; it simply cannot have its fixities resolved.
findProjectRoot :: FilePath -> IO (Maybe ProjectRoot)
findProjectRoot start = quietly Nothing $ do
  from <- startingDirectory
  found <- traverse markersIn (from : ancestorsOf from)
  pure (listToMaybe (concatMap fst found <> concatMap snd found))
  where
    startingDirectory = do
      absolute <- canonicalizePath start
      isDirectory <- doesDirectoryExist absolute
      pure (if isDirectory then absolute else takeDirectory absolute)

    ancestorsOf directory =
      let parent = takeDirectory directory
       in if parent == directory then [] else parent : ancestorsOf parent

    markersIn directory = quietly ([], []) $ do
      entries <- listDirectory directory
      pure
        ( [ProjectRoot directory ProjectFile | "cabal.project" `elem` entries],
          [ ProjectRoot directory (PackageFile named)
          | named <- take 1 (filter (".cabal" `isSuffixOf`) entries)
          ]
        )
