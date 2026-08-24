{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Documentation comments.
--
-- A Haddock is a comment that the syntax tree also knows about, which makes
-- it the one comment the printer places itself rather than leaving to
-- attachment. It has to: @-- ^ x@ documents what precedes it and @-- | x@
-- what follows, so moving the construct moves the Haddock, and where it ends
-- up cannot be worked out from where it started.
--
-- What the author wrote is reused whenever it can be, because rebuilding a
-- Haddock from the doc string the tree carries loses things the tree never
-- had: a @{- | … -}@ comes back as @-- |@ lines, and an empty @-- |@ comes
-- back as nothing at all. It cannot always be reused, since a trailing
-- @-- ^ x@ that is being moved in front of what it documents has to become
-- @-- | x@ or it will point at the wrong thing.
module Tilia.Render.Haddock
  ( DocStyle (..),
    Ending (..),
    haddock,
    haddockInline,
    docSectionName,
    brokenIfDocumented,
    printsWholeLineDocs,
    haddockSpans,
  )
where

import Data.Data (Data)
import Data.Generics.Schemes (listify)
import Control.Applicative ((<|>))
import Data.List (dropWhileEnd)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs
import GHC.Types.SrcLoc (GenLocated (..), getLoc, unLoc)
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Span
import Tilia.Span.Ghc

-- | Which kind of Haddock is being printed.
data DocStyle
  = -- | @-- |@, documenting what follows
    Pipe
  | -- | @-- ^@, documenting what precedes
    Caret
  | -- | @-- *@, a section heading, at the given depth
    Section Int
  | -- | @-- $name@, a named chunk
    Chunk String
  deriving (Eq, Show)

-- | Whether the Haddock ends the line it is on.
data Ending
  = -- | The caller will end the line itself.
    Open
  | -- | End it here.
    Closed
  deriving (Eq, Show)

-- | Print a Haddock.
haddock :: Ctx -> DocStyle -> Ending -> LHsDoc GhcPs -> Doc
haddock ctx style ending doc = case docBody ctx style doc of
  Nothing -> mempty
  Just (body, _) -> body <> close
  where
    close = case ending of
      Open -> mempty
      Closed -> hardBreak

-- | A Haddock inside a construct that may legitimately stay on one line.
--
-- A @{- | … -}@ delimits itself, so @data A = A {- | a number -} Int@ is
-- left as written. A @--@ Haddock owns the rest of its line and still has to
-- end it.
haddockInline :: Ctx -> DocStyle -> LHsDoc GhcPs -> Doc
haddockInline ctx style doc = case docBody ctx style doc of
  Nothing -> mempty
  Just (body, isSelfClosing) ->
    body <> (if isSelfClosing then breakOrSpace else hardBreak)

-- | The Haddock itself, and whether the form it took delimits itself.
docBody :: Ctx -> DocStyle -> LHsDoc GhcPs -> Maybe (Doc, Bool)
docBody ctx style doc@(L l str) =
  case reusableText ctx style doc of
    Just written ->
      Just
        ( maybe id located (spanOfSrcSpan l) $
            align (sepBy (verbatimBreak AtIndent) (map txt (NE.toList written))),
          isBlockForm written
        )
    Nothing
      | null (docLines str) -> Nothing
      | otherwise -> Just (rebuilt, False)
  where
    rebuilt =
      maybe id located (spanOfSrcSpan l) $
        sepBy hardBreak (zipWith line' (True : repeat False) (docLines str))
    line' isFirst t =
      (if isFirst then txt (opener style) else txt "--")
        <> space
        <> txt t

-- | How a rebuilt Haddock begins.
opener :: DocStyle -> Text
opener = \case
  Pipe -> "-- |"
  Caret -> "-- ^"
  Section n -> "-- " <> T.replicate n "*"
  Chunk n -> docSectionName n

-- | The anchor of a named documentation chunk.
--
-- Unlike a Haddock this carries no text of its own, so there is nothing to
-- reuse and nothing to report a position for.
docSectionName :: String -> Text
docSectionName n = "-- $" <> T.pack n

-- | The author's own text, when it may be used.
--
-- It may not when the Haddock is about to be printed in a style other than
-- the one it was written in, since the text carries the style in its first
-- characters.
reusableText :: Ctx -> DocStyle -> LHsDoc GhcPs -> Maybe (NonEmpty Text)
reusableText ctx style doc = do
  written <- writtenHaddock ctx (spanOfSrcSpan (getLoc doc))
  if writtenAs style (NE.head written) then Just written else Nothing

-- | Was the Haddock written in the style it is about to come back out in?
writtenAs :: DocStyle -> Text -> Bool
writtenAs style firstLine = case marker style of
  (wanted, tooMany) ->
    any (starts wanted tooMany) (T.stripPrefix "--" leader <|> T.stripPrefix "{-" leader)
  where
    leader = T.stripStart firstLine
    starts wanted tooMany rest =
      wanted `T.isPrefixOf` t && not (tooMany `T.isPrefixOf` t)
      where
        t = T.stripStart rest

-- | The text a style is written with, and a longer one that would mean a
-- different style.
--
-- Only the section headings need the second: @** x@ is a heading one level
-- down and not a @* x@ that happens to be followed by an asterisk. The rest
-- cannot be mistaken for anything longer, and say so with a marker no text
-- begins with.
marker :: DocStyle -> (Text, Text)
marker = \case
  Pipe -> ("|", noSuchText)
  Caret -> ("^", noSuchText)
  Section n -> (T.replicate n "*", T.replicate (n + 1) "*")
  Chunk n -> ("$" <> T.pack n, noSuchText)
  where
    noSuchText = "\0"

-- | Was the reused text a block comment?
isBlockForm :: NonEmpty Text -> Bool
isBlockForm written = "{-" `T.isPrefixOf` T.stripStart (NE.head written)

----------------------------------------------------------------------------
-- Documentation and layout

-- | Lay the document out on several lines if printing this fragment will
-- emit a Haddock that takes whole lines.
brokenIfDocumented :: (Data a) => Ctx -> a -> Doc -> Doc
brokenIfDocumented ctx x d
  | printsWholeLineDocs ctx x = broken d
  | otherwise = d

-- | Will printing this fragment emit a Haddock as @--@ lines?
--
-- Every site that asks prints in 'Pipe' style, which is what decides
-- whether the author's text can be reused.
printsWholeLineDocs :: (Data a) => Ctx -> a -> Bool
printsWholeLineDocs ctx x = case docsIn x of
  [] -> not (null (docStringsIn x))
  docs -> any takesWholeLines docs
  where
    takesWholeLines doc = case reusableText ctx Pipe doc of
      Just written -> not (isBlockForm written)
      Nothing -> not (null (docLines (unLoc doc)))

-- | The spans of every Haddock in a fragment.
--
-- Attachment must not place these: the printer has already put them where
-- they belong, and a comment placed twice is worse than one placed badly.
haddockSpans :: (Data a) => a -> [Span]
haddockSpans x = mapMaybe (spanOfSrcSpan . getLoc) (docsIn x) <> namedSections x

docsIn :: (Data a) => a -> [LHsDoc GhcPs]
docsIn = listify (const True :: LHsDoc GhcPs -> Bool)

-- | The spans of the @-- $name@ anchors in an export list.
--
-- These are the one kind of Haddock the syntax tree records without a doc
-- string: an anchor carries only its name, so there is no 'LHsDoc' to find
-- it by, and the item that holds it is the only record of where it was.
-- Without this the anchor is printed once from the tree and once more by
-- attachment, and each pass adds another copy.
namedSections :: (Data a) => a -> [Span]
namedSections =
  mapMaybe anchorSpan . listify (const True :: LIE GhcPs -> Bool)
  where
    anchorSpan l = case unLoc l of
      IEDocNamed {} -> spanOfSrcSpan (getHasLoc (getLoc l))
      _ -> Nothing

docStringsIn :: (Data a) => a -> [HsDocString]
docStringsIn = listify (const True :: HsDocString -> Bool)

----------------------------------------------------------------------------
-- Doc strings

-- | The lines of a doc string, normalised the way Haddock reads them.
docLines :: WithHsDocIdentifiers HsDocString GhcPs -> [Text]
docLines str
  | null body = []
  | otherwise = map (guardDollar . unpad) body
  where
    body =
      dropWhileEnd T.null
        . map (T.stripEnd . T.pack)
        . lines
        . renderHsDocString
        $ hsDocString str

    unpad t
      | padded, Just (' ', rest) <- T.uncons t = rest
      | otherwise = t
    padded = case dropWhile T.null body of
      (t : _) -> " " `T.isPrefixOf` t
      [] -> False

    -- A line may not begin with a dollar: that is the spelling of a named
    -- chunk, and one appearing by accident is a parse error.
    guardDollar t
      | "$" `T.isPrefixOf` t = T.cons '\\' t
      | otherwise = t
