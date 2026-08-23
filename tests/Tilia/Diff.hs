{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Showing what changed.
module Tilia.Diff
  ( Colours (..),
    coloursFor,
    diff,
  )
where

import Data.Algorithm.Diff qualified as D
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stdout)

----------------------------------------------------------------------------
-- Colour

-- | Whether to colour the output.
data Colours = Colours | Plain

-- | Colour a diff when there is somebody there to see it.
--
-- Escape codes written to a file or a pipe are noise in a log, so they are
-- emitted only for a terminal, and not then if @NO_COLOR@ is set.
coloursFor :: IO Colours
coloursFor = do
  refused <- lookupEnv "NO_COLOR"
  terminal <- hIsTerminalDevice stdout
  pure $
    if terminal && not (isJust refused)
      then Colours
      else Plain

data Ink = Meta | Gone | New | Unchanged

-- | Colour one line, and only that line.
paint :: Colours -> Ink -> Text -> Text
paint Plain _ t = t
paint Colours ink t = code <> t <> "\ESC[0m"
  where
    code = case ink of
      Meta -> "\ESC[36m"
      Gone -> "\ESC[31m"
      New -> "\ESC[32m"
      Unchanged -> "\ESC[39m"

----------------------------------------------------------------------------
-- Diffing

-- | One line of the comparison, with the number it has on each side.
data Line = Line !Mark !Int !Int !Text

data Mark = Context | Removed | Added
  deriving (Eq)

-- | A unified diff of two texts.
diff ::
  Colours ->
  -- | What to call the two sides
  (Text, Text) ->
  -- | Before
  Text ->
  -- | After
  Text ->
  Text
diff colours (beforeName, afterName) before after
  | null hunks =
      "(the two are identical as text, so the difference is in something\
      \ the text does not show)"
  | otherwise = T.intercalate "\n" (heading <> shown)
  where
    heading =
      [ paint colours Gone ("--- " <> beforeName),
        paint colours New ("+++ " <> afterName)
      ]

    shown
      | length body > roomFor = take roomFor body <> [omitted]
      | otherwise = body
      where
        omitted =
          paint colours Meta $
            "… and " <> T.pack (show (length body - roomFor)) <> " more lines"

    body = concatMap render hunks

    render range@(from, _) =
      hunkHeading range : map line (slice range)
      where
        _ = from

    hunkHeading range =
      paint colours Meta $
        "@@ -"
          <> span' beforeOf (countingBefore (slice range))
          <> " +"
          <> span' afterOf (countingAfter (slice range))
          <> " @@"
      where
        span' which n = case slice range of
          (l : _) -> T.pack (show (which l)) <> "," <> T.pack (show n)
          [] -> "0,0"

    -- The marker is not padded away from an empty line, so that a diff does
    -- not itself leave the trailing whitespace it is often being read to
    -- find.
    line (Line mark _ _ text) = case mark of
      Context -> paint colours Unchanged (marked "" "  ")
      Removed -> paint colours Gone (marked "-" "- ")
      Added -> paint colours New (marked "+" "+ ")
      where
        marked bare prefix
          | T.null text = bare
          | otherwise = prefix <> text

    slice (from, to) = take (to - from + 1) (drop from lines')

    countingBefore = length . filter (\(Line m _ _ _) -> m /= Added)
    countingAfter = length . filter (\(Line m _ _ _) -> m /= Removed)
    beforeOf (Line _ b _ _) = b
    afterOf (Line _ _ a _) = a

    hunks = merge [(max 0 (i - margin), min (total - 1) (i + margin)) | i <- changed]
    changed = [i | (i, Line m _ _ _) <- zip [0 ..] lines', m /= Context]
    total = length lines'
    lines' = tag (D.getGroupedDiff (split before) (split after))

    -- @T.lines@ drops a trailing empty line, and whether the output ends in
    -- a newline is exactly the sort of thing worth seeing.
    split = T.splitOn "\n"

-- | How many unchanged lines to show either side of a change.
margin :: Int
margin = 3

-- | How many lines of diff are worth printing before it stops being read.
roomFor :: Int
roomFor = 60

-- | Join hunks that have grown into one another.
merge :: [(Int, Int)] -> [(Int, Int)]
merge = \case
  ((a, b) : (c, d) : rest)
    | c <= b + 1 -> merge ((a, max b d) : rest)
    | otherwise -> (a, b) : merge ((c, d) : rest)
  xs -> xs

-- | Number the lines of a grouped diff on both sides at once.
tag :: [D.Diff [Text]] -> [Line]
tag = go 1 1
  where
    go _ _ [] = []
    go !b !a (d : ds) = case d of
      D.Both xs _ ->
        [Line Context (b + i) (a + i) x | (i, x) <- zip [0 ..] xs]
          <> go (b + length xs) (a + length xs) ds
      D.First xs ->
        [Line Removed (b + i) a x | (i, x) <- zip [0 ..] xs]
          <> go (b + length xs) a ds
      D.Second xs ->
        [Line Added b (a + i) x | (i, x) <- zip [0 ..] xs]
          <> go b (a + length xs) ds
