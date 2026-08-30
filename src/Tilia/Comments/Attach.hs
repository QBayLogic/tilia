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

import Data.Bifunctor (first, second)
import Data.List (mapAccumL, unsnoc)
import Data.Maybe (listToMaybe)
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
    (written, left) = walk (placeComments regions fences cs) doc
    (regions, fences) = markedSpans doc

-- | The spans of every 'DLocated' in the document, and of every 'DFence',
-- in that order.
markedSpans :: Doc -> ([Span], [Span])
markedSpans = \case
  DLocated s d -> first (s :) (markedSpans d)
  DFence s d -> second (s :) (markedSpans d)
  DCat a b -> markedSpans a <> markedSpans b
  DNest _ d -> markedSpans d
  DAlign d -> markedSpans d
  DGroup _ d -> markedSpans d
  DVariant a _ -> markedSpans a
  DCppChoice bs e -> foldMap (markedSpans . snd) bs <> markedSpans e
  _ -> ([], [])

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
            write position cs =
              foldMap (writtenAs (endOfAConstruct s) position) cs
            before' = heldOffFrom d [c | (q, c) <- mine, q == Before]
            after' = [c | (q, c) <- mine, q == After]
         in (write Before before' <> DLocated s d' <> write After after', p'')
      DFence s d -> first (DFence s) (go p d)
      DCppChoice bs e ->
        let branch q (c, d) = let (d', q') = go q d in (q', (c, d'))
            (p', bs') = mapAccumL branch p bs
            (e', p'') = go p' e
         in (DCppChoice bs' e', p'')
      DNest n d -> first (DNest n) (go p d)
      DAlign d -> first DAlign (go p d)
      DGroup l d -> first (DGroup l) (go p d)
      DVariant a b ->
        let (a', p') = go p a
            (b', _) = go p b
         in (DVariant a' b', p')
      d -> (d, p)

-- | Hold the last comment off a Haddock about to be written under it.
--
-- Only a comment written as @--@ lines needs holding off: the lexer would
-- read it and the Haddock under it as one comment. A @{- … -}@ ends at its
-- own bracket and may sit against whatever follows.
heldOffFrom :: Doc -> [Comment] -> [Comment]
heldOffFrom d cs = case unsnoc cs of
  Just (earlier, c)
    | not (bracketed c),
      opensWithHaddock d ->
        earlier <> [c {commentGapBelow = True}]
  _ -> cs

-- | Does this region begin its first line with a Haddock?
opensWithHaddock :: Doc -> Bool
opensWithHaddock = maybe False opensHaddock . listToMaybe . fst . firstLine Broken
  where
    firstLine layout = \case
      DText t -> ([t], False)
      DCat a b -> case firstLine layout a of
        (before, True) -> (before, True)
        (before, False) -> first (before <>) (firstLine layout b)
      DNest _ x -> firstLine layout x
      DAlign x -> firstLine layout x
      DLocated _ x -> firstLine layout x
      DFence _ x -> firstLine layout x
      DGroup l x -> firstLine l x
      DVariant a b -> firstLine layout (case layout of Flat -> a; Broken -> b)
      DHardBreak -> ([], True)
      DCloseLine -> ([], True)
      DBreak -> ([], layout == Broken)
      DSoftBreak -> ([], layout == Broken)
      _ -> ([], False)

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
writtenAs atTheEnd position c = case shapeOf position c of
  InPlace -> case position of
    Before -> includeWhen (not (commentTrailing c)) space <> commentDoc c <> space
    After -> space <> commentDoc c <> space
  EndsTheLine -> space <> commentDoc c <> closeLine
  HeldBack -> holdBack (renderComment c)
  OnItsOwnLines ->
    gapAbove <> closeLine <> commentDoc c <> closeLine <> gapBelow
  where
    gapAbove = includeWhen (commentGapAbove c) (closeLine <> blankLine)
    gapBelow = includeWhen (commentGapBelow c && not atTheEnd) blankLine

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
