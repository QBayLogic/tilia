{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Version (showVersion)
import Options.Applicative
import Paths_tilia (version)
import System.Directory (makeRelativeToCurrentDirectory)
import System.Exit (ExitCode (..))
import System.Exit qualified
import System.IO (hFlush, stderr, stdout)
import Tilia.Palette (Color (Bad), Palette, paletteFor)
import Tilia.Format
  ( FormatError,
    describeFormatError,
    formatErrorExitCode,
    newSession,
  )
import Tilia.Project (findProjectRoot)
import Tilia.Parser (ghcLibParserVersion)
import Tilia.Utils (lineWidth)
import Tilia.Run
  ( Outcome,
    Report (..),
    checkReport,
    differs,
    exitCodeOf,
    inplaceReport,
    noted,
    runOver,
    writeBack,
  )
import Tilia.Target
  ( Target,
    componentsOfTarget,
    describeTargetProblem,
    filesOfComponents,
    parseTarget,
  )

-- | The program's entry point.
main :: IO ()
main = do
  Opts {..} <- customExecParser (prefs (columns lineWidth)) optsParserInfo
  palette <- paletteFor
  target <- either
    (die usageExitCode palette)
    pure
    (maybe (parseTarget "all") parseTarget optTarget)
  files <- filesFor palette target
  session <- newSession "." >>= either (dieFormatting palette) pure
  outcomes <- runOver session files
  case optMode of
    Inplace -> do
      traverse_ writeBack outcomes
      printReport (inplaceReport palette outcomes)
    Check -> printReport (checkReport palette outcomes)
  exitWith optMode outcomes

-- | Exit the way the run turned out.
exitWith :: Mode -> [(FilePath, Outcome)] -> IO ()
exitWith mode outcomes = case exitCodeOf outcomes of
  Just code -> System.Exit.exitWith (ExitFailure code)
  Nothing -> case mode of
    Inplace -> pure ()
    Check -> when (any (differs . snd) outcomes) (System.Exit.exitWith (ExitFailure 1))

-- | Print a 'Report'.
printReport :: Report -> IO ()
printReport report = do
  traverse_ T.putStrLn (reportOut report)
  hFlush stdout
  traverse_ (T.hPutStrLn stderr) (reportErr report)
  hFlush stderr

-- | Every file the target asks for, relative to the current directory.
filesFor :: Palette -> Target -> IO [FilePath]
filesFor palette target =
  findProjectRoot "." >>= \case
    Nothing ->
      die 2 palette "no cabal.project, stack.yaml or .cabal file at or above the working directory"
    Just root ->
      componentsOfTarget root target >>= \case
        Left problem -> die usageExitCode palette (describeTargetProblem problem)
        Right components -> do
          found <- filesOfComponents components
          traverse makeRelativeToCurrentDirectory found

-- | What @sysexits.h@ has called a usage error since 4.3BSD, and well clear
-- of the codes 'formatErrorExitCode' returns.
usageExitCode :: Int
usageExitCode = 64

-- | Give up, under the same mark a failed file wears.
die :: Int -> Palette -> Text -> IO a
die code palette why = do
  traverse_ (T.hPutStrLn stderr) (noted palette ("✗", Bad) why)
  System.Exit.exitWith (ExitFailure code)

-- | Print out the 'FormatError' and exit.
dieFormatting :: Palette -> FormatError -> IO a
dieFormatting palette e =
  die (formatErrorExitCode e) palette (describeFormatError palette e)

----------------------------------------------------------------------------
-- Command line options

-- | What a run was asked to do.
data Mode = Inplace | Check

-- | The options a run was given.
data Opts = Opts
  { -- | The mode of operation.
    optMode :: Mode,
    -- | Which component to work on, if not all of them.
    optTarget :: Maybe String
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
        ("tilia " ++ showVersion version ++ "\nusing ghc-lib-parser " ++ ghcLibParserVersion)
        (long "version" <> short 'v' <> help "Print version of the program")

optsParser :: Parser Opts
optsParser =
  hsubparser . mconcat $
    [ command "inplace" (info (parser Inplace) (progDesc "Format files, in place")),
      command "check" (info (parser Check) (progDesc "Report what formatting would change, and fail if anything would"))
    ]
  where
    parser mode = Opts mode <$> optional targetArgument
    targetArgument =
      (strArgument . mconcat)
        [ metavar "TARGET",
          help
            "Component to format: all, a package or component name, or one of\
            \ lib:, exe:, test:, bench: followed by a name. Defaults to all."
        ]
