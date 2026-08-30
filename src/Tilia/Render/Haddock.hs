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
import Data.Maybe (fromMaybe, mapMaybe)
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
          selfClosing written
        )
    Nothing
      | null written' -> Nothing
      | blockForm -> Just (rebuiltBlock, False)
      | otherwise -> Just (rebuilt, False)
  where
    -- No provenance on a rebuilt Haddock, unlike one whose text is reused.
    -- Rebuilding is what happens when the author wrote it in another style,
    -- and the commonest of those is a @-- ^@ being printed as @-- |@, which
    -- moves it from after what it documents to before. Offering where it
    -- used to be as somewhere a comment may attach would put that comment
    -- ahead of comments that were written above it.
    rebuilt =
      sepBy hardBreak (zipWith line' (True : repeat False) written')
        <> mconcat (replicate trailingBlanks (hardBreak <> txt "--"))
    trailingBlanks = case writtenHaddock ctx (spanOfSrcSpan l) of
      Nothing -> 0
      Just ls -> length (takeWhile isBlankLine (reverse (NE.toList ls)))
    isBlankLine t = T.null (T.strip (fromMaybe t (T.stripPrefix "--" (T.strip t))))

    line' isFirst t =
      (if isFirst then txt (opener style) else txt "--")
        <> space
        <> txt t

    -- One the author wrote as a block comment over several lines is
    -- rebuilt as one. Cut into @--@ lines it would stop being a single
    -- comment: the lexer reads the first line as documentation and every
    -- line after it as an ordinary comment, so a Haddock of two lines would
    -- come back as a Haddock of one and a comment saying half a sentence.
    -- A block of one line has no such lines to lose and is rebuilt as
    -- @-- |@ like any other.
    rebuiltBlock =
      align $
        txt (blockOpener style)
          <> space
          <> sepBy (verbatimBreak AtIndent) (map txt written')
          <> space
          <> txt "-}"

    asBlock = writtenAsBlock ctx doc
    written' = docLines asBlock str
    blockForm = asBlock && length written' > 1

-- | How a rebuilt Haddock begins.
opener :: DocStyle -> Text
opener = \case
  Pipe -> "-- |"
  Caret -> "-- ^"
  Section n -> "-- " <> T.replicate n "*"
  Chunk n -> docSectionName n

-- | How a rebuilt Haddock that stays a block comment begins.
blockOpener :: DocStyle -> Text
blockOpener = \case
  Pipe -> "{- |"
  Caret -> "{- ^"
  Section n -> "{- " <> T.replicate n "*"
  Chunk n -> "{- $" <> T.pack n

-- | Did the author write this Haddock as a block comment?
writtenAsBlock :: Ctx -> LHsDoc GhcPs -> Bool
writtenAsBlock ctx doc =
  maybe False isBlockForm (writtenHaddock ctx (spanOfSrcSpan (getLoc doc)))

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
  if openedInStyle style (NE.head written) then Just written else Nothing

-- | Was the Haddock written in the style it is about to come back out in?
openedInStyle :: DocStyle -> Text -> Bool
openedInStyle style firstLine = case afterOpener firstLine of
  Nothing -> False
  Just inside -> case style of
    -- A chunk's name is the compiler's to delimit, and it may have stopped
    -- somewhere the line carries on: @-- $Id: …@ names the chunk @Id@ and
    -- then goes on with a colon that is no part of it. So the name is
    -- matched as a prefix and where it ends is left to the compiler.
    Chunk _ -> triggerFor style `T.isPrefixOf` inside
    -- The rest are a run of characters that ends where the run ends, so the
    -- run is read off the line and compared whole. Matching a prefix would
    -- take @** x@ for a @* x@ that happens to be followed by a star.
    _ -> triggerOn inside == Just (triggerFor style)

-- | The trigger a style is written with.
triggerFor :: DocStyle -> Text
triggerFor = \case
  Pipe -> "|"
  Caret -> "^"
  Section n -> T.replicate n "*"
  Chunk n -> "$" <> T.pack n

-- | What follows the @--@ or @{-@ that opens a comment, with the spaces
-- after it removed.
afterOpener :: Text -> Maybe Text
afterOpener firstLine = T.stripStart <$> opened (T.stripStart firstLine)
  where
    opened t = T.stripPrefix "--" t <|> T.stripPrefix "{-" t

-- | The trigger an opened comment carries, for the triggers that are a run
-- of one character.
triggerOn :: Text -> Maybe Text
triggerOn inside = do
  (c, rest) <- T.uncons inside
  case c of
    '|' -> Just "|"
    '^' -> Just "^"
    '*' -> Just (T.cons c (T.takeWhile (== '*') rest))
    _ -> Nothing

-- | Was the reused text a block comment?
isBlockForm :: NonEmpty Text -> Bool
isBlockForm written = "{-" `T.isPrefixOf` T.stripStart (NE.head written)

-- | May code follow the reused text on the line it ends?
selfClosing :: NonEmpty Text -> Bool
selfClosing written = isBlockForm written && null (NE.tail written)

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
      Just written -> not (selfClosing written)
      Nothing -> not (null (docLines (writtenAsBlock ctx doc) (unLoc doc)))

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
docLines ::
  -- | Was it written as a block comment?
  Bool ->
  WithHsDocIdentifiers HsDocString GhcPs ->
  [Text]
docLines blockForm str
  | null body = []
  | otherwise = map guardDollar (dedent (map unpad body))
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

    -- Written as @{- | … -}@, the lines after the first are indented to sit
    -- under the opening bracket, and that indentation is measured from a
    -- column the text is about to leave: printed back as @--@ lines it
    -- would show up as a run of spaces the author never typed. Only the
    -- part they all share goes, so anything indented further—an example, a
    -- code block—keeps the shape it was given.
    dedent ls
      | not blockForm = ls
      | otherwise = case ls of
          [] -> []
          (first' : rest) -> first' : map (T.drop (shared rest)) rest

    shared ls = case map indentation (filter (not . T.null) ls) of
      [] -> 0
      ns -> minimum ns
    indentation = T.length . T.takeWhile (== ' ')

    -- A line may not begin with a dollar: that is the spelling of a named
    -- chunk, and one appearing by accident is a parse error.
    guardDollar t
      | "$" `T.isPrefixOf` t = T.cons '\\' t
      | otherwise = t
