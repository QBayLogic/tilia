{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Putting comments into the document.
--
-- Attachment happens once, on the finished document, before anything is
-- rendered. A comment becomes an ordinary part of the document like any
-- other, and from then on nothing distinguishes it.
module Tilia.Comments.Attach
  ( attachComments,
    commentDoc,
  )
where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Tilia.Comments
import Tilia.Doc.Combinators
import Tilia.Doc.Internal (Doc (..))
import Tilia.Span

-- | Put every comment into the document.
--
-- Comments are taken in source order and matched against the located nodes
-- in the order the document mentions them. Any that are left over are
-- appended at the end rather than dropped.
attachComments :: [Comment] -> Doc -> Doc
attachComments cs doc =
  case insert (locatedSpans doc) (sortComments cs) doc of
    (doc', []) -> doc'
    (doc', leftover) -> doc' <> below doc' leftover
  where
    sortComments = foldr ins []
    ins c [] = [c]
    ins c (x : xs)
      | startsBefore (commentSpan c) (commentSpan x) = c : x : xs
      | otherwise = x : ins c xs

-- | Every region the document records provenance for.
--
-- Collected before anything is placed, because deciding where a comment
-- goes needs to know what else could hold it, and the walk that places them
-- only ever sees what it has reached so far.
locatedSpans :: Doc -> [Span]
locatedSpans = \case
  DLocated s d -> s : locatedSpans d
  DCat a b -> locatedSpans a <> locatedSpans b
  DNest _ d -> locatedSpans d
  DAlign d -> locatedSpans d
  DGroup _ d -> locatedSpans d
  -- The two branches of a variant hold the same nodes, so one is enough.
  DVariant a _ -> locatedSpans a
  _ -> []

-- | Walk the document, consuming comments as located nodes go past.
--
-- The list is threaded left to right and only ever shrinks, which is what
-- makes duplication impossible: a comment is removed from it at the moment
-- it is placed.
insert :: [Span] -> [Comment] -> Doc -> (Doc, [Comment])
insert everywhere = go
  where
    go cs = \case
      DCat a b ->
        let (a', cs') = go cs a
            (b', cs'') = go cs' b
         in (DCat a' b', cs'')
      DLocated s d ->
        let (before, rest) = span (\c -> startsBefore (commentSpan c) s) cs
            (inside, rest') = span (\c -> commentSpan c `within` s) rest
            (d', unplaced) = go inside d
            (trailing, rest'') = span (\c -> trailsOnly s (commentSpan c)) rest'
            body = DLocated s (d' <> below d' unplaced)
         in ( above before <> body <> trailingDoc s trailing,
              rest''
            )
      DNest n d -> first (DNest n) (go cs d)
      DAlign d -> first DAlign (go cs d)
      DGroup l d -> first (DGroup l) (go cs d)
      DVariant a b ->
        let (a', cs') = go cs a
            (b', _) = go cs b
         in (DVariant a' b', cs')
      d -> (d, cs)
    trailsOnly s c =
      trails c s && not (any holdsIt everywhere) && not (any nearer everywhere)
      where
        holdsIt other = c `within` other && not (s `within` other)
        nearer other = trails c other && endsAfter other s
        endsAfter a b =
          (spanEndLine a, spanEndColumn a) > (spanEndLine b, spanEndColumn b)

-- | Comments preceding an element.
--
-- Where a comment goes is decided by one thing: whether the author wrote it
-- after code on its line. One that did shares a line here too, and one that
-- had a line to itself gets one. That is not merely a nice rule, it is the
-- rule that makes formatting settle: the property it reads off the input is
-- exactly the property the output has, so a second pass reaches the same
-- answer as the first.
above :: [Comment] -> Doc
above = foldMap one
  where
    one c = gapAbove c <> place c <> gapBelow c
    gapAbove c =
      includeWhen
        (ownsTheLine c && not (commentTrailing c) && commentAfterGap c)
        (closeLine <> blankLine)
    gapBelow c = includeWhen (ownsTheLine c && commentBeforeGap c) blankLine

-- | Comments on lines of their own, below whatever precedes them.
below ::
  -- | What was printed above them here
  Doc ->
  [Comment] ->
  Doc
below printed = foldMap one
  where
    one c = closeLine <> gapAbove <> commentDoc c <> closeLine
    gapAbove = case printed of
      DEmpty -> mempty
      _ -> blankLine

-- | Comments that follow an element on the line it ends on.
--
-- Unlike a comment that precedes something, one of these cannot simply be
-- written out where it stands: the element it trails is very often not the
-- last thing on its line—a record field is followed by a comma, a pattern
-- by an arrow, a list by its closing bracket—and ending the line here would
-- push all of that onto the next one. Handing it to the engine as a line
-- ending says what is meant, and lets the printer carry on emitting.
trailingDoc :: Span -> [Comment] -> Doc
trailingDoc _ = foldMap one
  where
    one c
      | not (ownsTheLine c) = space <> commentDoc c <> space
      | singleLine c = holdBack (renderComment c)
      | otherwise = space <> commentDoc c <> closeLine

-- | One comment, put where its own shape says it belongs.
place :: Comment -> Doc
place c
  | inline c = commentDoc c <> space
  | commentTrailing c = space <> commentDoc c <> closeLine
  | otherwise = closeLine <> commentDoc c <> closeLine

-- | Was the comment written among code rather than above it?
inline :: Comment -> Bool
inline c = not (ownsTheLine c) && (commentTrailing c || commentFollowed c)

-- | Is this comment a single line?
singleLine :: Comment -> Bool
singleLine c = case commentBody c of
  (_ :| []) -> True
  _ -> False

-- | Does this comment take the rest of the line it lands on?
--
-- A @{- … -}@ written on one line does not: it closes itself, so code may
-- follow it. Anything else does, and whatever comes after it has to start a
-- new line.
ownsTheLine :: Comment -> Bool
ownsTheLine c = case commentStyle c of
  BlockComment -> not (singleLine c)
  _ -> True

-- | A comment as a document.
--
-- The continuation lines of a block comment line up under its opener rather
-- than under the enclosing indentation. The opener may end up anywhere on
-- its line, and what the author arranged was the shape of the comment, not
-- its distance from the left margin.
commentDoc :: Comment -> Doc
commentDoc c =
  located (commentSpan c) . align $
    sepBy (verbatimBreak AtIndent) (map txt (NE.toList (commentBody c)))

----------------------------------------------------------------------------
-- Span relations

-- | Does the first span begin before the second?
startsBefore :: Span -> Span -> Bool
startsBefore a b =
  (spanStartLine a, spanStartColumn a) < (spanStartLine b, spanStartColumn b)

-- | Does the first span fall inside the second?
within :: Span -> Span -> Bool
within a b =
  (spanStartLine b, spanStartColumn b) <= (spanStartLine a, spanStartColumn a)
    && (spanEndLine a, spanEndColumn a) <= (spanEndLine b, spanEndColumn b)

-- | Does the first span follow the second on the same line, or just below
-- it?
trails :: Span -> Span -> Bool
trails a b =
  spanStartLine a == spanEndLine b
    && (spanStartLine a, spanStartColumn a) >= (spanEndLine b, spanEndColumn b)
