{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Formatting a file, with everything the project can tell us about it.
module Tilia.Format
  ( FormatError (..),
    describeFormatError,
    formatFile,
    formatIn,
  )
where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Tilia.Fixity.Plan (loadPlan, newResolver, scopeFor)
import Tilia.Parser
  ( ParseError,
    defaultParserConfig,
    describeParseError,
    parseText,
    pmModule,
    effectiveExtensions,
  )
import Tilia.Doc (defaultRenderOptions, printDoc)
import Tilia.Project (ProjectRoot (..), findProjectRoot)
import Tilia.Render (Settings (..), defaultSettings, renderModule)

-- | Why a file could not be formatted.
data FormatError
  = -- | No @cabal.project@, @stack.yaml@ or @.cabal@ file above it.
    NoProject FilePath
  | -- | A project, but no build plan we could read or produce. The text is
    -- whatever @cabal@ had to say about it.
    NoBuildPlan FilePath Text
  | -- | The file is not Haskell we can parse.
    NotParsed ParseError

-- | Say what went wrong, in one line.
describeFormatError :: FormatError -> Text
describeFormatError = \case
  NoProject path ->
    "no project above " <> T.pack path <> ": expected a cabal.project, a stack.yaml or a .cabal file"
  NoBuildPlan root reason ->
    "no build plan for " <> T.pack root <> ": " <> reason
  NotParsed e -> "cannot parse " <> describeParseError e

-- | Format a file, using the project it belongs to.
formatFile :: FilePath -> IO (Either FormatError Text)
formatFile path = T.readFile path >>= formatIn path

-- | Format text that belongs where the given path does.
formatIn :: FilePath -> Text -> IO (Either FormatError Text)
formatIn path source =
  findProjectRoot path >>= \case
    Nothing -> pure (Left (NoProject path))
    Just root ->
      loadPlan (prPath root) >>= \case
        Left reason -> pure (Left (NoBuildPlan (prPath root) reason))
        Right plan -> case parseText defaultParserConfig path source of
          Left e -> pure (Left (NotParsed e))
          Right parsed -> do
            resolve <- newResolver plan
            scope <- scopeFor resolve (pmModule parsed)
            let settings =
                  defaultSettings
                    { setExtensions = Set.fromList (effectiveExtensions source),
                      setScope = Just scope
                    }
            pure (Right (printDoc defaultRenderOptions (renderModule settings parsed)))
