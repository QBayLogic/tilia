{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Putting comments into the document.
--
-- Attachment happens once, on the finished document, before anything is
-- rendered. A comment becomes an ordinary part of the document like any
-- other, and from then on nothing distinguishes it.
module Linden.Comments.Attach
  ( attachComments,
    commentDoc,
  )
where

import Data.Bifunctor (first)
import Data.List.NonEmpty qualified as NE
import Linden.Comments
import Linden.Printer.Combinators
import Linden.Printer.Internal (Doc (..))

-- | Put every comment into the document.
--
-- Comments are taken in source order and matched against the located nodes
-- in the order the document mentions them. Any that are left over are
-- appended at the end rather than dropped.
attachComments :: [Comment] -> Doc -> Doc
attachComments cs doc =
  case insert (sortComments cs) doc of
    (doc', []) -> doc'
    (doc', leftover) -> doc' <> below leftover
  where
    sortComments = foldr ins []
    ins c [] = [c]
    ins c (x : xs)
      | startsBefore (commentSpan c) (commentSpan x) = c : x : xs
      | otherwise = x : ins c xs

-- | Walk the document, consuming comments as located nodes go past.
--
-- The list is threaded left to right and only ever shrinks, which is what
-- makes duplication impossible: a comment is removed from it at the moment
-- it is placed.
insert :: [Comment] -> Doc -> (Doc, [Comment])
insert cs = \case
  DCat a b ->
    let (a', cs') = insert cs a
        (b', cs'') = insert cs' b
     in (DCat a' b', cs'')
  DLocated s d ->
    let (before, rest) = span (\c -> startsBefore (commentSpan c) s) cs
        (inside, rest') = span (\c -> commentSpan c `within` s) rest
        (d', unplaced) = insert inside d
        (trailing, rest'') = span (\c -> trails (commentSpan c) s) rest'
        body = DLocated s (d' <> below unplaced)
     in ( above before <> body <> trailingDoc s trailing,
          rest''
        )
  DNest n d -> first (DNest n) (insert cs d)
  DAlign d -> first DAlign (insert cs d)
  DGroup l d -> first (DGroup l) (insert cs d)
  DVariant a b ->
    let (a', cs') = insert cs a
        (b', _) = insert cs b
     in (DVariant a' b', cs')
  d -> (d, cs)

-- | Comments on lines of their own, above whatever follows them.
above :: [Comment] -> Doc
above = foldMap (\c -> commentDoc c <> hardBreak)

-- | Comments on lines of their own, below whatever precedes them.
below :: [Comment] -> Doc
below [] = mempty
below cs = foldMap (\c -> hardBreak <> commentDoc c) cs <> hardBreak

-- | Comments that follow an element.
trailingDoc :: Span -> [Comment] -> Doc
trailingDoc s = go True
  where
    go _ [] = mempty
    go isFirst (c : rest) =
      lead isFirst c <> commentDoc c <> hardBreak <> go False rest
    lead False _ = mempty
    lead True c
      | spanStartLine (commentSpan c) == spanEndLine s = space
      | otherwise = hardBreak

-- | A comment as a document.
commentDoc :: Comment -> Doc
commentDoc c =
  located (commentSpan c) $
    sepBy hardBreak (map txt (NE.toList (commentBody c)))

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
