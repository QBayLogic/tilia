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
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Tilia.Comments (Comment (..))
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
placeComments :: [Span] -> [Comment] -> Placements
placeComments regions comments =
  Placements
    { placedAt = Map.fromListWith (flip (<>)) (mapMaybe assign comments),
      placedNowhere = filter (isNothing . against) comments
    }
  where
    assign c = do
      (region, position) <- against c
      pure (region, [(position, c)])

    against c
      | commentTrailing c, Just r <- trailed = Just (r, Trailing)
      | Just r <- next = Just (r, Above)
      | otherwise = Nothing
      where
        here = commentSpan c
        trailed =
          nearest (\r -> (Down (endPoint r), startPoint r)) $
            filter (\r -> endsJustBefore r && not (fencedOff r)) regions
        endsJustBefore r =
          spanEndLine r == spanStartLine here && endPoint r <= startPoint here
        fencedOff r = any (\o -> here `inside` o && not (r `inside` o)) regions
        next =
          nearest (\r -> (startPoint r, Down (endPoint r))) $
            filter (\r -> startPoint r >= endPoint here) regions

    nearest :: (Ord k) => (Span -> k) -> [Span] -> Maybe Span
    nearest key = listToMaybe . sortOn key

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
