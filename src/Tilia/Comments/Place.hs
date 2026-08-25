-- | Deciding where each comment goes.
module Tilia.Comments.Place
  ( -- * Where a comment goes
    Position (..),

    -- * The answers
    Placements,
    placeComments,
    takePlaced,
    unplaced,
  )
where

import Data.List (sortOn)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Tilia.Comments (Comment (..), closesItself, commentTrailing)
import Tilia.Span

-- | Where a comment stands in relation to the region it was given to.
data Position
  = -- | On lines of its own, above the region.
    Above
  | -- | At the end of the line the region ends on.
    Trailing
  deriving (Eq, Ord, Show)

-- | What each region was given, and what nothing could be found for.
data Placements = Placements
  { placedAt :: Map Span [(Position, Comment)],
    placedNowhere :: [Comment]
  }

-- | Give every comment to a region.
placeComments ::
  -- | The regions a comment may be given to
  [Span] ->
  -- | The boundaries a comment printed in place may not be carried across
  [Span] ->
  [Comment] ->
  Placements
placeComments regions fences comments =
  Placements
    { placedAt = Map.fromListWith (flip (<>)) [(r, [(p, c)]) | (Just (r, p), c) <- decided],
      placedNowhere = [c | (Nothing, c) <- decided]
    }
  where
    decided = [(against c, c) | c <- comments]

    -- Which lines end in a comment, so that a comment lined up under one of
    -- them can tell whether it is carrying a remark on.
    linesEndingInAComment =
      IntSet.fromList
        [spanEndLine (commentSpan c) | c <- comments, commentTrailing c]

    against c
      | commentTrailing c, Just r <- trailed = Just (r, Trailing)
      | not (commentTrailing c), Just r <- continues = Just (r, Trailing)
      | Just r <- next = Just (r, Above)
      | otherwise = Nothing
      where
        here = commentSpan c
        trailed
          | writtenAgainst || not (commentFollowed c) = before
          | otherwise = Nothing
        before =
          nearest (\r -> (Down (endPoint r), startPoint r)) $
            filter (\r -> endsJustBefore r && not (fencedOff r)) regions

        writtenAgainst =
          any (\r -> Just (endPoint r) == stopsAt) regions
        stopsAt = (,) (spanStartLine here) <$> commentCodeBeforeStopsAt c

        endsJustBefore r =
          spanEndLine r == spanStartLine here && endPoint r <= startPoint here

        fencedOff r = apart regions || (closesItself c && apart fences)
          where
            apart = any (\o -> here `inside` o && not (r `inside` o))

        next =
          nearest (\r -> (startPoint r, Down (endPoint r))) $
            filter (\r -> startPoint r >= endPoint here) regions

        continues
          | Just column <- commentContentAboveAt c,
            column == spanStartColumn here,
            runsOnFromAbove,
            nothingBelowItLinesUp =
              endingAbove
          | otherwise = Nothing

        runsOnFromAbove =
          IntSet.member (spanStartLine here - 1) linesEndingInAComment

        nothingBelowItLinesUp =
          all (\r -> spanStartColumn r < spanStartColumn here) next

        endingAbove =
          nearest (\r -> (Down (endPoint r), startPoint r)) $
            filter endsOnTheLineAbove regions
          where
            endsOnTheLineAbove r =
              spanEndLine r == spanStartLine here - 1 && not (fencedOff r)

    -- Folded rather than sorted: this runs for every comment against every
    -- region, and only the first of the order is ever wanted.
    nearest :: (Ord k) => (Span -> k) -> [Span] -> Maybe Span
    nearest key = fmap fst . foldl' closer Nothing
      where
        closer best s = case best of
          Just (_, k) | k <= key s -> best
          _ -> Just (s, key s)

-- | Does the first region fall within the second?
inside :: Span -> Span -> Bool
inside a b = startPoint b <= startPoint a && endPoint a <= endPoint b

-- | Take what a region was given, so that nothing can take it again.
takePlaced :: Span -> Placements -> ([(Position, Comment)], Placements)
takePlaced s p = case Map.updateLookupWithKey forget s (placedAt p) of
  (found, rest) -> (concat found, p {placedAt = rest})
  where
    forget _ _ = Nothing

-- | The comments no region ever came to collect.
unplaced :: Placements -> [Comment]
unplaced p =
  sortOn (startPoint . commentSpan) $
    placedNowhere p <> [c | (_, c) <- concat (Map.elems (placedAt p))]
