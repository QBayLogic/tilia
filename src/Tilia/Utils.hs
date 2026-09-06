{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Miscellaneous utilities.
module Tilia.Utils
  ( quietly,
    attempted,
    lineWidth,
    indent,
    wrapTo,
    visibleLength,
  )
where

import Control.Exception (SomeException, displayException, try)
import Data.Text (Text)
import Data.Text qualified as T

-- | Run an action, falling back on the given value if it throws.
quietly :: a -> IO a -> IO a
quietly fallback action =
  try action >>= \case
    Left (_ :: SomeException) -> pure fallback
    Right a -> pure a

-- | Run an action, keeping what it threw rather than a fallback.
attempted :: IO a -> IO (Either Text a)
attempted action =
  try action >>= \case
    Left (e :: SomeException) -> pure (Left (T.pack (displayException e)))
    Right a -> pure (Right a)

-- | The widest line this program will print of its own accord.
lineWidth :: Int
lineWidth = 76

-- | Two spaces per level, which is what everything here is set in.
indent :: Int -> Text
indent level = T.replicate (2 * level) " "

-- | Break text into lines that fit the room given, at spaces.
wrapTo :: Int -> Text -> [Text]
wrapTo room = concatMap (go . T.words) . T.lines
  where
    go [] = []
    go (w : ws) = let (line, rest) = fill w ws in line : go rest
    fill line (w : ws)
      | visibleLength line + 1 + visibleLength w <= room = fill (line <> " " <> w) ws
    fill line ws = (line, ws)

-- | How wide a piece of text is once printed.
visibleLength :: Text -> Int
visibleLength = go 0
  where
    go !n t = case T.uncons t of
      Nothing -> n
      Just ('\ESC', rest)
        | Just after <- T.stripPrefix "[" rest -> go n (T.drop 1 (T.dropWhile (/= 'm') after))
      Just (_, rest) -> go (n + 1) rest
