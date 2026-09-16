{-# LANGUAGE OverloadedStrings #-}

-- | Handling of newlines.
module Tilia.Newline
  ( NewlineStyle (..),
    getNewlineStyle,
    setNewlineStyle,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

-- | The two ways a line can end.
data NewlineStyle
  = -- | @\\n@, as everything but Windows writes them.
    Lf
  | -- | @\\r\\n@, as Windows writes them.
    CrLf
  deriving (Eq, Show)

-- | Which of the two a text is written with, by the first ending in it.
getNewlineStyle :: Text -> NewlineStyle
getNewlineStyle t = case T.breakOn "\n" t of
  (before, rest)
    | not (T.null rest),
      "\r" `T.isSuffixOf` before ->
        CrLf
  _ -> Lf

-- | Set every line ending in a text the given way.
setNewlineStyle :: NewlineStyle -> Text -> Text
setNewlineStyle style = case style of
  Lf -> toNewlines
  CrLf -> T.replace "\n" "\r\n" . toNewlines
  where
    toNewlines = T.replace "\r\n" "\n"
