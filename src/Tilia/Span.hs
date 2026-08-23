-- | Regions of the input, and the questions asked about them.
--
-- Deliberately not GHC's @RealSrcSpan@. Almost everything here wants to ask
-- one of a handful of questions—did this occupy a single line, did that
-- begin on the line this ended on, was there an empty line between them—and
-- a type of our own keeps the modules that ask them free of the compiler's
-- libraries. Conversion happens at the edge, in "Tilia.Span.Ghc", which is
-- the only place that has to know what GHC's positions look like.
module Tilia.Span
  ( -- * Spans
    Span (..),
    mkSpan,

    -- * Asking about one
    isSingleLine,

    -- * Asking about two
    sameLine,
    blankBetween,

    -- * Narrowing
    startOf,
    endOf,
  )
where

-- | A region of the input.
data Span = Span
  { spanStartLine :: !Int,
    spanStartColumn :: !Int,
    spanEndLine :: !Int,
    spanEndColumn :: !Int
  }
  deriving (Eq, Show)

-- | Build a 'Span' from start and end positions, each a line and a column.
mkSpan :: (Int, Int) -> (Int, Int) -> Span
mkSpan (sl, sc) (el, ec) = Span sl sc el ec

-- | The smallest span covering both arguments.
--
-- Printing code needs this often enough—a construct the syntax tree has no
-- single node for still has to be laid out as a unit—that it is worth having
-- as an instance rather than as a function each caller reimplements.
instance Semigroup Span where
  a <> b =
    Span
      { spanStartLine = min (spanStartLine a) (spanStartLine b),
        spanStartColumn = case compare (spanStartLine a) (spanStartLine b) of
          LT -> spanStartColumn a
          GT -> spanStartColumn b
          EQ -> min (spanStartColumn a) (spanStartColumn b),
        spanEndLine = max (spanEndLine a) (spanEndLine b),
        spanEndColumn = case compare (spanEndLine a) (spanEndLine b) of
          GT -> spanEndColumn a
          LT -> spanEndColumn b
          EQ -> max (spanEndColumn a) (spanEndColumn b)
      }

-- | Did this occupy a single line of the input?
--
-- The question the whole formatter turns on: what was written on one line
-- stays on one line, and what was spread out stays spread out.
isSingleLine :: Span -> Bool
isSingleLine s = spanStartLine s == spanEndLine s

-- | Did the second thing begin on the line the first thing ended on?
--
-- This is the question behind almost every hanging decision: a body may only
-- hang off what precedes it when the author had them starting together.
sameLine :: Maybe Span -> Maybe Span -> Bool
sameLine (Just a) (Just b) = spanEndLine a == spanStartLine b
sameLine _ _ = False

-- | Was there an empty line between the two?
blankBetween :: Maybe Span -> Maybe Span -> Bool
blankBetween (Just a) (Just b) = spanStartLine b > spanEndLine a + 1
blankBetween _ _ = False

-- | A zero-width span at the start of the given one.
startOf :: Span -> Span
startOf s = at (spanStartLine s, spanStartColumn s)

-- | A zero-width span at the end of the given one.
endOf :: Span -> Span
endOf s = at (spanEndLine s, spanEndColumn s)

at :: (Int, Int) -> Span
at position = mkSpan position position
