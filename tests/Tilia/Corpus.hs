{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Corpora of Haskell that other people wrote.
module Tilia.Corpus
  ( -- * Corpora
    Corpus (..),
    Reference (..),
    ormoluExamples,
    ghcTestSuite,

    -- * Obtaining one
    Example (..),
    obtain,
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Exception (SomeException, try)
import Control.Monad (forM)
import Data.ByteString.Lazy qualified as BL
import Data.List (isPrefixOf, isSuffixOf, sort, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory
  ( XdgDirectory (..),
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getXdgDirectory,
    listDirectory,
    removePathForcibly,
    renameDirectory,
    renameFile,
  )
import System.Environment (lookupEnv)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Req
import System.FilePath (splitDirectories, takeDirectory, (</>))

----------------------------------------------------------------------------
-- Corpora

-- | Whether a corpus says what the formatted result should look like.
data Reference
  = -- | It does not, so only the properties that hold of any input can be
    -- checked.
    NoReference
  | -- | It does, in a file whose name is the input's with @.hs@ replaced by
    -- the given suffix. A file already carrying that suffix is its own
    -- reference: it is what the formatter is supposed to settle on.
    ReferenceSuffix String
  deriving (Eq, Show)

-- | Where a corpus comes from and what is in it.
data Corpus = Corpus
  { -- | Used for the cache directory and in test names.
    corpusName :: String,
    -- | Where to fetch a @.tar.gz@ with a single directory at the top, and
    -- whatever query it needs.
    --
    -- Built rather than written out as a string, so that a query the
    -- download depends on cannot be lost in an edit that looks harmless.
    corpusUrl :: (Url 'Https, Option 'Https),
    -- | The directory inside the archive holding the examples.
    corpusRoot :: FilePath,
    -- | Whether the corpus says what the formatted result should look like.
    corpusReference :: Reference,
    -- | Examples to leave alone, named relative to 'corpusRoot'. A name
    -- with no extension stands for a directory and takes everything under
    -- it.
    corpusSkip :: [FilePath]
  }

-- | Ormolu's examples.
ormoluExamples :: Corpus
ormoluExamples =
  Corpus
    { corpusName = "ormolu-0.9.0.0",
      corpusUrl =
        ( https "hackage.haskell.org"
            /: "package"
            /: "ormolu-0.9.0.0"
            /: "ormolu-0.9.0.0.tar.gz",
          mempty
        ),
      corpusRoot = "data" </> "examples",
      corpusReference = ReferenceSuffix "-out.hs",
      corpusSkip =
        [ "other" </> "disabling"
        ]
    }

-- | GHC's test suite.
ghcTestSuite :: Corpus
ghcTestSuite =
  Corpus
    { corpusName = "ghc-9.10.1-testsuite",
      corpusUrl =
        ( https "gitlab.haskell.org"
            /: "ghc"
            /: "ghc"
            /: "-"
            /: "archive"
            /: "ghc-9.10.1-release"
            /: "ghc.tar.gz",
          "path" =: ("testsuite/tests" :: Text)
        ),
      corpusRoot = "testsuite" </> "tests",
      corpusReference = NoReference,
      corpusSkip =
        [ "perf" </> "compiler" </> "parsing001.hs"
        ]
    }

----------------------------------------------------------------------------
-- Obtaining one

-- | One thing to format, and what it should come out as if that is known.
data Example = Example
  { -- | Where the corpus puts it, relative to the corpus root, which is
    -- what names the test: the absolute path runs through a cache directory
    -- that differs on every machine.
    exampleName :: FilePath,
    -- | The file to format, as an absolute path on this machine.
    exampleInput :: FilePath,
    -- | The file holding what the corpus says formatting should produce, if
    -- it says. 'Nothing' for a corpus that ships no expected outputs, and
    -- for an example within one that happens to have none.
    exampleReference :: Maybe FilePath
  }
  deriving (Eq, Show)

-- | Get a corpus, fetching and unpacking it if this machine does not have
-- it yet.
--
-- The work happens once: an unpacked corpus is left in place and found
-- again, and a download interrupted half way leaves nothing behind to be
-- mistaken for a complete one. 'Left' is for the machine that cannot reach
-- the network rather than for a defect, and callers are expected to say so
-- and carry on rather than fail.
obtain :: Corpus -> IO (Either Text [Example])
obtain corpus = do
  home <- corpusCache
  let archive = home </> corpusName corpus <> ".tar.gz"
      unpacked = home </> corpusName corpus
  createDirectoryIfMissing True home
  ready <- doesDirectoryExist unpacked
  prepared <-
    if ready
      then pure (Right ())
      else do
        have <- doesFileExist archive
        got <-
          if have
          then pure (Right ())
          else download (corpusUrl corpus) archive
        either (pure . Left) (const (unpackTo archive unpacked)) got
  case prepared of
    Left problem -> pure (Left problem)
    Right () -> Right <$> examplesIn corpus (unpacked </> corpusRoot corpus)

-- | Where corpora are kept.
--
-- Beside the fixity cache, and for the same reason: it is data about the
-- outside world that is expensive to obtain and cheap to keep.
corpusCache :: IO FilePath
corpusCache =
  lookupEnv "TILIA_CORPUS_DIR" >>= \case
    Just dir -> pure dir
    Nothing -> (</> "corpus") <$> getXdgDirectory XdgCache "tilia"

----------------------------------------------------------------------------
-- Fetching

-- | Fetch an archive.
--
-- Written to a temporary name and moved into place. Anything that leaves a
-- partial file under the real name would be taken for a complete download
-- on the next run and never fetched again.
--
-- 'Left' is for the machine that cannot reach the network rather than for a
-- defect, and callers are expected to say so and carry on. Only the fetch
-- is caught: a file that cannot be written is a fault worth hearing about.
download :: (Url 'Https, Option 'Https) -> FilePath -> IO (Either Text ())
download (url, query) dest =
  try fetch >>= \case
    Left (e :: HttpException) -> pure (Left (explain e))
    Right bytes -> do
      BL.writeFile partial bytes
      Right <$> renameFile partial dest
  where
    partial = dest <> ".part"
    fetch =
      runReq defaultHttpConfig $
        responseBody <$> req GET url NoReqBody lbsResponse query
    explain = \case
      VanillaHttpException (HTTP.HttpExceptionRequest _ reason) -> flatten reason
      other -> flatten other
    flatten :: (Show a) => a -> Text
    flatten = T.take 200 . T.unwords . T.words . T.pack . show

-- | Unpack the Haskell files of an archive, dropping its top-level
-- directory.
unpackTo :: FilePath -> FilePath -> IO (Either Text ())
unpackTo archive dest = do
  removePathForcibly staging
  outcome <- quietly (Left "could not unpack") $ do
    bytes <- BL.readFile archive
    Tar.foldEntries write (pure ()) (const (pure ())) (Tar.read (GZip.decompress bytes))
    pure (Right ())
  case outcome of
    Left problem -> do
      removePathForcibly staging
      pure (Left (problem <> " " <> T.pack archive))
    Right () -> do
      there <- doesDirectoryExist staging
      if there
        then Right <$> renameDirectory staging dest
        else pure (Left ("nothing to unpack in " <> T.pack archive))
  where
    staging = dest <> ".part"

    write entry rest = do
      case Tar.entryContent entry of
        Tar.NormalFile content _
          | Just path <- beneathTop (Tar.entryPath entry),
            ".hs" `isSuffixOf` path -> do
              createDirectoryIfMissing True (takeDirectory (staging </> path))
              BL.writeFile (staging </> path) content
        _ -> pure ()
      rest
    beneathTop path = case splitDirectories path of
      (_ : rest@(_ : _)) | all safe rest -> Just (foldr1 (</>) rest)
      _ -> Nothing
    safe part = part /= ".." && not ("/" `isPrefixOf` part)

----------------------------------------------------------------------------
-- Enumerating

-- | Every example in an unpacked corpus, in a settled order.
examplesIn :: Corpus -> FilePath -> IO [Example]
examplesIn corpus root = do
  found <- sort <$> haskellFilesIn root
  let files = filter (not . skipped) found
  case corpusReference corpus of
    NoReference -> pure [Example (nameOf f) f Nothing | f <- files]
    ReferenceSuffix suffix -> forM files $ \f ->
      if suffix `isSuffixOf` f
        then pure (Example (nameOf f) f (Just f))
        else do
          let reference = take (length f - length (".hs" :: String)) f <> suffix
          there <- doesFileExist reference
          pure (Example (nameOf f) f (if there then Just reference else Nothing))
  where
    nameOf f = fromMaybe f (stripPrefix (root <> "/") f)
    -- A skipped name matches an example outright or stands for the
    -- directory it is in.
    skipped f = any covers (corpusSkip corpus)
      where
        name = nameOf f
        covers entry = entry == name || (entry <> "/") `isPrefixOf` name

haskellFilesIn :: FilePath -> IO [FilePath]
haskellFilesIn dir = do
  isDir <- doesDirectoryExist dir
  if not isDir
    then pure [dir | ".hs" `isSuffixOf` dir]
    else do
      entries <- quietly [] (listDirectory dir)
      concat <$> traverse (haskellFilesIn . (dir </>)) entries

----------------------------------------------------------------------------
-- Helpers

quietly :: a -> IO a -> IO a
quietly fallback action =
  try action >>= \case
    Left (_ :: SomeException) -> pure fallback
    Right a -> pure a
