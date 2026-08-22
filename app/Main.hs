{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Text.IO qualified as T
import Data.Version (showVersion)
import Options.Applicative
import Paths_tilia (version)
import Tilia (tilia)

main :: IO ()
main = do
  Opts {..} <- execParser optsParserInfo
  input <- maybe T.getContents T.readFile optInputFile
  T.putStr (tilia input)

----------------------------------------------------------------------------
-- Command line options parsing

-- | Command line options.
newtype Opts = Opts
  { -- | File to format, 'Nothing' means stdin
    optInputFile :: Maybe FilePath
  }

optsParserInfo :: ParserInfo Opts
optsParserInfo =
  info (helper <*> versionOption <*> optsParser) . mconcat $
    [ fullDesc,
      progDesc "Format Haskell source code",
      header "tilia - a formatter for Haskell source code"
    ]
  where
    versionOption =
      infoOption
        ("tilia " ++ showVersion version)
        (long "version" <> short 'v' <> help "Print version of the program")

optsParser :: Parser Opts
optsParser =
  Opts
    <$> (optional . strArgument . mconcat)
      [ metavar "FILE",
        help "Haskell source file to format or stdin (default)"
      ]
