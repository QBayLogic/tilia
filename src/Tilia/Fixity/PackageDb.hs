{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Which package exposes a module, according to the compiler.
--
-- The other way of answering this — reading @exposed-modules@ from a
-- @.cabal@ file inside a source tarball — only works where tarballs are.
-- Under Nix they are not: dependencies arrive already built, and a plan
-- solved there calls almost all of them @pre-existing@, so nothing is ever
-- looked for.
--
-- The compiler always knows, though, because it is what compiles against
-- them. Asking @ghc-pkg@ works in both worlds and is the faster of the two.
--
-- What this does /not/ give is fixities. A package database records what
-- was built, not what it was built from, so the source is still read from a
-- tarball; this only decides which tarball to look for.
module Tilia.Fixity.PackageDb
  ( InstalledPackage (..),
    Installed (..),
    readInstalledPackages,
  )
where

import Control.Monad (filterM)
import Data.Char (isSpace)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Tilia.Utils (quietly)

-- | A package the compiler can see.
data InstalledPackage = InstalledPackage
  { -- | Package name
    ipName :: Text,
    -- | Package version
    ipVersion :: Text,
    -- | Every module it holds, hidden ones included, with re-export clauses
    -- dropped: those name modules belonging to another package, and looking
    -- there is that package's business.
    ipModules :: [Text],
    -- | Where its compiled interfaces are.
    ipImportDirs :: [FilePath]
  }
  deriving (Eq, Show)

-- | What the compiler can see, and where it is.
data Installed = Installed
  { -- | Every package it can see.
    installedPackages :: [InstalledPackage],
    installedDatabases :: [FilePath]
  }
  deriving (Eq, Show)

-- | Everything the compiler can see, and where it read it from.
--
-- Empty if @ghc-pkg@ cannot be run, which is not fatal.
--
-- @ghc-pkg@ is invoked rather than a database read off disk because where
-- the databases are is not knowable from outside: under Nix the wrapper
-- carries the paths, and @GHC_PACKAGE_PATH@ is not set. The records name
-- them, though, so having asked once we need not ask again to find out
-- whether the answer still holds.
readInstalledPackages :: IO Installed
readInstalledPackages = quietly (Installed [] []) $ do
  (code, out, _) <- readProcessWithExitCode "ghc-pkg" ["dump", "--global", "--user"] ""
  case code of
    ExitSuccess -> do
      let fields = map parseFields (records (T.pack out))
      databases <- filterM doesDirectoryExist (databasesIn fields)
      pure
        Installed
          { installedPackages = mapMaybe fromFields fields,
            installedDatabases = databases
          }
    _ -> pure (Installed [] [])

-- | The databases a set of records came out of.
databasesIn :: [Map.Map Text Text] -> [FilePath]
databasesIn fields =
  nub
    [ T.unpack (unquote root) </> "package.conf.d"
    | f <- fields,
      Just root <- [Map.lookup "pkgroot" f]
    ]

-- | Strip the quotes a path is written in when it has none needing them.
unquote :: Text -> Text
unquote = T.dropAround (== '"') . T.strip

-- | Split @ghc-pkg dump@ output into its records.
records :: Text -> [Text]
records = map T.unlines . go . T.lines
  where
    go ls = case break (== "---") ls of
      (record, []) -> [record | not (null record)]
      (record, _ : rest) -> record : go rest

-- | Read one record's fields, if they name a package.
fromFields :: Map.Map Text Text -> Maybe InstalledPackage
fromFields fields = do
  name <- Map.lookup "name" fields
  version <- Map.lookup "version" fields
  pure
    InstalledPackage
      { ipName = T.strip name,
        ipVersion = T.strip version,
        ipModules =
          concatMap
            (maybe [] moduleNames . (`Map.lookup` fields))
            ["exposed-modules", "hidden-modules"],
        ipImportDirs =
          maybe [] (map (T.unpack . unquote) . T.words) (Map.lookup "import-dirs" fields)
      }

-- | The module names in an @exposed-modules@ field.
moduleNames :: Text -> [Text]
moduleNames = go . filter (not . T.null) . concatMap (T.split (== ',')) . T.words
  where
    go = \case
      (_ : "from" : _ : rest) -> go rest
      (m : rest) | looksLikeModule m -> m : go rest
      (_ : rest) -> go rest
      [] -> []
    looksLikeModule m = case T.uncons m of
      Just (c, _) -> c `elem` ['A' .. 'Z'] && not (T.any (== ':') m)
      Nothing -> False

-- | Split a record into its fields.
--
-- A field is @name: value@, and its value continues onto any following
-- indented lines.
parseFields :: Text -> Map.Map Text Text
parseFields = Map.fromList . mapMaybe field . groups . T.lines
  where
    groups = \case
      [] -> []
      (l : ls)
        | isContinuation l -> groups ls
        | otherwise ->
            let (continued, rest) = span isContinuation ls
             in (l : continued) : groups rest
    isContinuation l = not (T.null l) && isSpace (T.head l)

    field [] = Nothing
    field (l : rest) = case T.breakOn ":" l of
      (key, value)
        | not (T.null value) ->
            Just (T.strip key, T.unwords (T.drop 1 value : map T.strip rest))
      _ -> Nothing
