{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Putting comments into the document.
--
-- Attachment happens once, on the finished document, before anything is
-- rendered. A comment becomes an ordinary part of the document like any
-- other, and from then on nothing distinguishes it.
module Tilia.Comments.Attach
  ( attachComments,
  )
where

import Data.Bifunctor (first)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Tilia.Comments
import Tilia.Comments.Place
import Tilia.Doc.Combinators
import Tilia.Doc.Internal (Doc (..))
import Tilia.Span

-- | Put every comment into the document.
attachComments :: [Comment] -> Doc -> Doc
attachComments cs doc = written <> foldMap atEnd (unplaced left)
  where
    (written, left) = walk (placeComments (locatedSpans doc) cs) doc

-- | Every region the document records provenance for.
locatedSpans :: Doc -> [Span]
locatedSpans = \case
  DLocated s d -> s : locatedSpans d
  DCat a b -> locatedSpans a <> locatedSpans b
  DNest _ d -> locatedSpans d
  DAlign d -> locatedSpans d
  DGroup _ d -> locatedSpans d
  DVariant a _ -> locatedSpans a
  _ -> []

-- | Walk the document, giving each region what it was given.
walk :: Placements -> Doc -> (Doc, Placements)
walk = go
  where
    go p = \case
      DCat a b ->
        let (a', p') = go p a
            (b', p'') = go p' b
         in (DCat a' b', p'')
      DLocated s d ->
        let (mine, p') = takePlaced s p
            (d', p'') = go p' d
            only' = only (endOfAConstruct s) mine
         in (only' Above <> DLocated s d' <> only' Trailing, p'')
      DNest n d -> first (DNest n) (go p d)
      DAlign d -> first DAlign (go p d)
      DGroup l d -> first (DGroup l) (go p d)
      DVariant a b ->
        let (a', p') = go p a
            (b', _) = go p b
         in (DVariant a' b', p')
      d -> (d, p)

    only atTheEnd mine position =
      foldMap (writtenAs atTheEnd position) [c | (q, c) <- mine, q == position]

-- | Does this region stand for where a construct stops rather than for
-- anything written?
endOfAConstruct :: Span -> Bool
endOfAConstruct s = startPoint s == endPoint s

----------------------------------------------------------------------------
-- What a comment looks like

-- | One comment, written where it was placed.
writtenAs ::
  -- | Does what follows only mark where the construct ends?
  Bool ->
  Position ->
  Comment ->
  Doc
writtenAs atTheEnd position c = case position of
  Above
    | closesItself c && commentFollowed c -> commentDoc c <> space
    | commentTrailing c -> space <> commentDoc c <> closeLine
    | otherwise -> gapAbove <> onItsOwnLine <> gapBelow
  Trailing
    | closesItself c -> space <> commentDoc c <> space
    | singleLine c -> holdBack (renderComment c)
    | otherwise -> space <> commentDoc c <> closeLine
  where
    onItsOwnLine = closeLine <> commentDoc c <> closeLine
    gapAbove = includeWhen (commentAfterGap c) (closeLine <> blankLine)
    gapBelow = includeWhen (commentBeforeGap c && not atTheEnd) blankLine

-- | A comment nothing came to collect, written after everything.
atEnd :: Comment -> Doc
atEnd c = closeLine <> blankLine <> commentDoc c <> closeLine

-- | A comment as a document.
commentDoc :: Comment -> Doc
commentDoc c =
  located (commentSpan c) . align $
    sepBy (verbatimBreak AtIndent) (map txt (NE.toList (commentBody c)))

----------------------------------------------------------------------------
-- The two document atoms that exist for comments

-- | Text put at the end of the line this position falls on.
--
-- The argument must not contain a line break.
holdBack :: Text -> Doc
holdBack = DHoldBack

-- | Close the line, absorbing a break that immediately follows.
closeLine :: Doc
closeLine = DCloseLine
