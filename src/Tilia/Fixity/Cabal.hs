{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reading a package's exposed modules out of its @.cabal@ file.
module Tilia.Fixity.Cabal
  ( packageModules,
    exposedModules,
    sourceDirs,
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Lazy qualified as BL
import Data.Char (isSpace)
import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Tilia.Utils (quietly)

-- | The modules a package exposes, read from the @.cabal@ file in its
-- source tarball.
--
-- Nothing if the tarball cannot be read or holds no @.cabal@ file.
packageModules :: FilePath -> IO (Maybe [Text])
packageModules tarball = quietly Nothing $ do
  bytes <- BL.readFile tarball
  pure (exposedModules <$> findCabalFile (Tar.read (GZip.decompress bytes)))

-- | The first @.cabal@ file at the top level of an archive.
--
-- Stops as soon as it finds one. The archive is decompressed lazily, so a
-- @.cabal@ near the front costs a fraction of the whole file.
findCabalFile :: Tar.Entries e -> Maybe Text
findCabalFile = \case
  Tar.Next entry rest
    | ".cabal" `isSuffixOf` Tar.entryPath entry,
      depth (Tar.entryPath entry) == 2,
      Tar.NormalFile content _ <- Tar.entryContent entry ->
        Just (T.decodeUtf8Lenient (BL.toStrict content))
    | otherwise -> findCabalFile rest
  Tar.Done -> Nothing
  Tar.Fail _ -> Nothing
  where
    -- Top level of the archive: @pkg-1.0/pkg.cabal@ and nothing deeper, so
    -- that a @.cabal@ bundled in a test fixture is not mistaken for it.
    depth path = 1 + length (filter (== '/') path)

-- | Every module named by an @exposed-modules@ field, from every branch of
-- every conditional, in every library stanza.
--
-- @other-modules@ is excluded: those cannot be imported, so their fixities
-- can never be in scope. @reexported-modules@ is excluded too, but for the
-- opposite reason—the modules it names live in another package, and looking
-- there is a separate step this does not take.
exposedModules :: Text -> [Text]
exposedModules =
  concatMap moduleNames . fieldsNamed "exposed-modules" . T.lines
  where
    moduleNames =
      filter looksLikeModule
        . concatMap (T.split (== ','))
        . T.words

    looksLikeModule m = case T.uncons m of
      Just (c, _) -> c `elem` ['A' .. 'Z']
      Nothing -> False

-- | Every directory named by an @hs-source-dirs@ field.
--
-- A package that names none keeps its modules beside the @.cabal@ file, so
-- the current directory is the answer rather than nothing.
sourceDirs :: Text -> [Text]
sourceDirs contents = case named of
  [] -> ["."]
  ds -> ds
  where
    named =
      filter (not . T.null)
        . map T.strip
        . concatMap (T.split (== ','))
        . concatMap T.words
        . fieldsNamed "hs-source-dirs"
        $ T.lines contents

-- | The values of every field with the given name, wherever it appears and
-- however deeply it is nested.
fieldsNamed :: Text -> [Text] -> [Text]
fieldsNamed name = go
  where
    go = \case
      [] -> []
      (l : ls)
        | Just value <- fieldValue l ->
            let (continued, rest) = span (deeperThan (indentOf l)) ls
             in T.unwords (value : map T.strip continued) : go rest
        | otherwise -> go ls

    fieldValue l =
      let (key, rest) = T.break (== ':') l
       in if T.toLower (T.strip key) == name && not (T.null rest)
            then Just (T.strip (T.drop 1 rest))
            else Nothing

    deeperThan n l = T.null (T.strip l) || indentOf l > n
    indentOf = T.length . T.takeWhile isSpace

