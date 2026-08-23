{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Formatting other people's Haskell.
module Tilia.CorpusSpec (spec) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (join)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Test.Hspec hiding (Example, after, before, example)
import Text.Read (readMaybe)
import Tilia.Corpus
import Tilia.Diff (Colours, coloursFor, diff)
import Tilia.Equivalence (commentDifference, syntaxDifference)
import Tilia.Parser
  ( ParsedModule (..),
    defaultParserConfig,
    parseText,
  )
import Tilia.Doc (defaultRenderOptions, printDoc)
import Tilia.Render (renderModule)
import Tilia.TestConfig (exampleSettings)

spec :: Spec
spec = do
  corpusSpec ormoluExamples
  corpusSpec ghcTestSuite

corpusSpec :: Corpus -> Spec
corpusSpec corpus =
  describe (corpusName corpus) $
    runIO (obtain corpus) >>= \case
      Left problem ->
        it "is available" . pendingWith $
          "corpus not on this machine and could not be fetched: " <> T.unpack problem
      Right examples -> do
        tally <- runIO (newIORef mempty)
        limit <- runIO exampleTimeout
        colours <- runIO coloursFor

        for_ examples $ \example ->
          it (exampleName example) $ do
            result <- check colours limit example
            modifyIORef' tally (<> record result)
            case result of
              Broken why -> expectationFailure (T.unpack why)
              _ -> pure ()

        summarise (length examples) tally

-- | How long one example gets before it is called a failure.
exampleTimeout :: IO Int
exampleTimeout =
  lookupEnv "TILIA_CORPUS_TIMEOUT" >>= \case
    Just s | Just seconds <- readMaybe s -> pure (seconds * 1_000_000)
    _ -> pure (60 * 1_000_000)

----------------------------------------------------------------------------
-- What the run added up to

-- | Counts across a corpus, accumulated as its examples run.
data Tally = Tally
  { -- | Examples we could read and format.
    tallyChecked :: !Int,
    -- | Examples that are not Haskell we can read.
    tallySkipped :: !Int
  }

instance Semigroup Tally where
  a <> b =
    Tally
      { tallyChecked = tallyChecked a + tallyChecked b,
        tallySkipped = tallySkipped a + tallySkipped b
      }

instance Monoid Tally where
  mempty = Tally 0 0

-- | What one result contributes.
--
-- A broken example still counts as checked: it was read and formatted, and
-- the failure is reported against the example itself.
record :: Result -> Tally
record = \case
  Skipped -> mempty {tallySkipped = 1}
  Broken _ -> mempty {tallyChecked = 1}
  Formatted -> mempty {tallyChecked = 1}

-- | What the run covered, as against what any one example did.
--
-- These run last, which is what makes the tally complete by the time they
-- read it.
summarise :: Int -> IORef Tally -> Spec
summarise total tally = do
  it "has examples to run" $ do
    Tally {..} <- readIORef tally
    -- Under a selection there may legitimately be nothing here; it is the
    -- whole corpus formatting nothing that would mean the wiring is broken.
    if tallyChecked + tallySkipped < total
      then pure ()
      else tallyChecked `shouldSatisfy` (> 0)

  it "reports what it covered" $ do
    Tally {..} <- readIORef tally
    pendingWith $
      show tallyChecked
        <> " examples formatted, "
        <> show tallySkipped
        <> " skipped as not parseable by us"

----------------------------------------------------------------------------
-- Checking one example

-- | What running the formatter over one example established.
data Result
  = -- | The input is not Haskell we can read, which is a fact about the
    -- corpus rather than about the formatter. GHC's test suite is full of
    -- files that are meant not to compile, and of CPP that we do not
    -- expand.
    Skipped
  | -- | A property does not hold.
    Broken Text
  | -- | Nothing was found wrong with it.
    Formatted

check :: Colours -> Int -> Example -> IO Result
check colours limit example = do
  source <- readUtf8 (exampleInput example)
  expected <- traverse readUtf8 (exampleReference example)
  case source of
    Nothing -> pure Skipped
    Just text -> guarded limit (checkPure colours text (join expected))

-- | Read a file that is supposed to be a Haskell module.
--
-- Decoded here rather than by 'T.readFile', which would use whatever
-- encoding the machine happens to be set to and fail on a byte it did not
-- expect. A Haskell source file is UTF-8 by definition, so one that is not
-- is not a module—GHC's test suite carries a handful on purpose, to check
-- that the compiler rejects them—and 'Nothing' puts it with the rest of the
-- corpus we cannot read instead of reporting it against the formatter.
readUtf8 :: FilePath -> IO (Maybe Text)
readUtf8 path = either (const Nothing) Just . decodeUtf8' <$> BS.readFile path

-- | Run a check, turning a crash or a hang into a failure rather than into a
-- dead test run.
--
-- The result is forced here rather than left to the assertion, so that an
-- error raised deep inside the printer is reported against the example that
-- provoked it, and so that the time it takes is time the limit covers.
guarded :: Int -> Result -> IO Result
guarded limit result =
  try (timeout limit (evaluate (forced result))) >>= \case
    Left (e :: SomeException) ->
      pure (Broken ("the formatter raised an error: " <> firstLine (T.pack (show e))))
    Right Nothing ->
      pure (Broken ("the formatter did not finish within " <> T.pack (show (limit `div` 1_000_000)) <> "s"))
    Right (Just settled) -> pure settled
  where
    forced = \case
      Skipped -> Skipped
      Broken why -> T.length why `seq` Broken why
      Formatted -> Formatted
    firstLine = T.strip . T.takeWhile (/= '\n')

-- | Everything that can be established about one example without doing any
-- more input or output.
--
-- Each failure carries a diff, and which two things are being compared is
-- chosen to be the pair the reader would have compared themselves. A
-- property that broke somewhere inside the module is shown against the
-- input, because the question is what formatting did to it. Formatting that
-- will not settle is shown against the first pass, because the first pass
-- is the fixed point it was supposed to have reached and everything they
-- agree on is beside the point.
checkPure :: Colours -> Text -> Maybe Text -> Result
checkPure colours source expected = case parse source of
  Nothing -> Skipped
  Just before ->
    let formatted = render before
        against name = diff colours ("input", name) source formatted
     in case parse formatted of
          Nothing ->
            Broken
              ( "the formatted output does not parse\n"
                  <> against "output (does not parse)"
              )
          Just after
            | Just difference <- syntaxDifference (pmModule before) (pmModule after) ->
                Broken
                  ( "a different program: "
                      <> difference
                      <> "\n"
                      <> against "output"
                  )
            | Just difference <- commentDifference (pmComments before) (pmComments after) ->
                Broken
                  ( "comments: "
                      <> difference
                      <> "\n"
                      <> against "output"
                  )
            | settled <- render after,
              settled /= formatted ->
                Broken
                  ( "formatting is non-idempotent\n"
                      <> diff colours ("first pass", "second pass") formatted settled
                  )
            -- A corpus that says what the answer should be is believed.
            -- There is no tolerance to spend: an example we lay out
            -- differently is one we have not finished with, whatever the
            -- reason, and a proportion of them being right says nothing
            -- about any particular one.
            | Just reference <- expected,
              reference /= formatted ->
                Broken
                  ( "does not match the corpus's expected output\n"
                      <> diff colours ("expected", "ours") reference formatted
                  )
            | otherwise -> Formatted
  where
    parse = either (const Nothing) Just . parseText defaultParserConfig "<corpus>"

    -- The settings are worked out from the module in hand rather than fixed
    -- once, because they depend on what it imports; see "Tilia.TestConfig".
    render parsed =
      printDoc
        defaultRenderOptions
        (renderModule (exampleSettings source (pmModule parsed)) parsed)
