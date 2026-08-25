{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Corpora of Haskell to run the formatter over.
module Tilia.Corpus
  ( -- * Corpora
    Corpus (..),
    Source (..),
    Reference (..),
    vendoredExamples,
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
import Data.Maybe (fromMaybe, maybeToList)
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

-- | Where the examples of a corpus come from.
data Source
  = -- | Fetched from the network and unpacked into a cache, once per
    -- machine.
    Fetched (Url 'Https, Option 'Https) FilePath
  | -- | Checked into this repository, so always at hand and never fetched.
    Vendored FilePath

-- | Where a corpus comes from and what is in it.
data Corpus = Corpus
  { -- | Used for the cache directory and in test names.
    corpusName :: String,
    -- | Where its examples come from.
    corpusSource :: Source,
    -- | Whether the corpus says what the formatted result should look like.
    corpusReference :: Reference,
    -- | Examples to leave alone, named relative to the root of the corpus.
    -- A name with no extension stands for a directory and takes everything
    -- under it.
    corpusSkip :: [FilePath],
    -- | Examples the formatter is supposed to refuse, named in full.
    corpusDeclined :: [FilePath]
  }

-- | Our own examples.
vendoredExamples :: Corpus
vendoredExamples =
  Corpus
    { corpusName = "tilia",
      corpusSource = Vendored "vendored-corpus",
      corpusReference = ReferenceSuffix "-out.hs",
      corpusSkip = [],
      corpusDeclined = ["other" </> "position-pragmas.hs"]
    }

-- | Ormolu's examples.
ormoluExamples :: Corpus
ormoluExamples =
  Corpus
    { corpusName = "ormolu-0.9.0.0",
      corpusSource =
        Fetched
          ( https "hackage.haskell.org"
              /: "package"
              /: "ormolu-0.9.0.0"
              /: "ormolu-0.9.0.0.tar.gz",
            mempty
          )
          ("data" </> "examples"),
      corpusReference = ReferenceSuffix "-out.hs",
      corpusSkip =
        [ "other" </> "disabling",
          "declaration" </> "value" </> "function" </> "required-type-arguments-2.hs",
          "declaration" </> "data" </> "comment-in-empty-record.hs",
          "import" </> "comment-inside-empty-import-list.hs",
          "other" </> "comment-two-blocks.hs",
          "other" </> "multiple-blank-line-comment.hs",
          "declaration" </> "type" </> "parens-comments.hs",
          "declaration" </> "value" </> "function" </> "parens-comments.hs",
          "import" </> "comments-inside-imports.hs",
          "import" </> "comment-between-merged-imports.hs",
          "declaration" </> "data" </> "with-comment.hs",
          "declaration" </> "data" </> "record-empty-haddock.hs",
          "other" </> "empty-haddock.hs",
          "declaration" </> "value" </> "function" </> "arrow" </> "proc-do-complex.hs",
          "declaration" </> "value" </> "function" </> "comprehension" </> "transform-multi-line2.hs",
          "declaration" </> "value" </> "function" </> "if-with-comment-next-to-keyword.hs",
          "declaration" </> "value" </> "function" </> "operator-comments-2.hs",
          "declaration" </> "value" </> "function" </> "record" </> "wildcard-comments-0.hs",
          "declaration" </> "value" </> "function" </> "record" </> "wildcard-comments-1.hs",
          "other" </> "pragma-comments-after.hs",
          "declaration" </> "value" </> "function" </> "infix" </> "esqueleto-0.hs",
          "declaration" </> "value" </> "function" </> "infix" </> "esqueleto-1.hs",
          "declaration" </> "class" </> "default-signatures.hs",
          "declaration" </> "type-families" </> "closed-type-family" </> "with-comments.hs"
        ],
      corpusDeclined = []
    }

-- | GHC's test suite.
ghcTestSuite :: Corpus
ghcTestSuite =
  Corpus
    { corpusName = "ghc-9.10.1-testsuite",
      corpusSource =
        Fetched
          ( https "gitlab.haskell.org"
              /: "ghc"
              /: "ghc"
              /: "-"
              /: "archive"
              /: "ghc-9.10.1-release"
              /: "ghc.tar.gz",
            "path" =: ("testsuite/tests" :: Text)
          )
          ("testsuite" </> "tests"),
      corpusReference = NoReference,
      corpusSkip =
        [ "perf" </> "compiler" </> "parsing001.hs"
        ],
      corpusDeclined =
        [ "ghci.debugger" </> "HappyTest.hs",
          "parser" </> "should_compile" </> "ColumnPragma.hs",
          "parser" </> "should_compile" </> "T7118.hs",
          "perf" </> "compiler" </> "T20261.hs",
          "perf" </> "compiler" </> "T5631.hs",
          "programs" </> "joao-circular" </> "Funcs_Parser_Lazy.hs"
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
-- Fetching happens once: an unpacked corpus is left in place and found
-- again, and a download interrupted half way leaves nothing behind to be
-- mistaken for a complete one. 'Left' is for the machine that cannot reach
-- the network rather than for a defect, and callers are expected to say so
-- and carry on rather than fail. A vendored corpus is already here and can
-- never fail this way.
obtain :: Corpus -> IO (Either Text [Example])
obtain corpus = case corpusSource corpus of
  Vendored dir -> Right <$> examplesIn corpus dir
  Fetched url root -> do
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
              else download url archive
          either (pure . Left) (const (unpackTo archive unpacked)) got
    case prepared of
      Left problem -> pure (Left problem)
      Right () -> Right <$> examplesIn corpus (unpacked </> root)

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
    skipped f = any listed (nameOf f : maybeToList (inputFor (nameOf f)))
    listed name = any covers (corpusSkip corpus)
      where
        covers entry = entry == name || (entry <> "/") `isPrefixOf` name
    inputFor name = case corpusReference corpus of
      ReferenceSuffix suffix
        | Just stem <- withoutSuffix suffix name -> Just (stem <> ".hs")
      _ -> Nothing
    withoutSuffix suffix name
      | suffix `isSuffixOf` name = Just (take (length name - length suffix) name)
      | otherwise = Nothing

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
