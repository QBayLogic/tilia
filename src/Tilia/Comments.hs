{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Extracting comments from a parsed module.
module Tilia.Comments
  ( Comment (..),
    CommentStyle (..),
    commentsOf,
    renderComment,
    closesItself,
    commentTrailing,
    singleLine,
    widenTrigger,
    escapeTrigger,
    triggerEscaped,
    documentsNothing,

    -- * Pragmas
    Pragma (..),
    commentPragma,
  )
where

import Data.Char (isSpace)
import Data.Generics.Schemes (listify)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs (HsModule)
import GHC.Hs.Extension (GhcPs)
import GHC.Parser.Annotation qualified as GHC
import GHC.Types.SrcLoc qualified as GHC
import Tilia.Span (Span)
import Tilia.Span.Ghc (spanOfReal)

-- | How a comment was written. The distinction is kept because it
-- constrains what may be done with the comment.
data CommentStyle
  = -- | @-- …@
    LineComment
  | -- | @{- … -}@
    BlockComment
  | -- | @-- |@, @-- ^@, @-- *@, @-- $@ and the block forms
    DocComment
  deriving (Eq, Show)

-- | One comment.
data Comment = Comment
  { -- | Where it was in the input.
    commentSpan :: Span,
    -- | Its lines, dedented, without trailing whitespace. A line comment
    -- has one; a block comment has one per line it spanned.
    commentBody :: NonEmpty Text,
    -- | How it was written.
    commentStyle :: CommentStyle,
    -- | Where the code before it on its opening line stops: the column one
    -- past the last character of that code, or 'Nothing' when the comment
    -- had the line to itself.
    commentFollows :: Maybe Int,
    -- | Whether the line above it in the input was empty.
    --
    -- What is above a comment is often not a node at all—a Haddock the
    -- printer emits from the syntax tree, another comment—so the gap cannot
    -- be worked out from spans later. It matters: a @-- |@ and a @--@ run
    -- together are one doc string, and separated by an empty line they are
    -- a doc string and a comment.
    commentAfterGap :: Bool,
    -- | Whether the line below it in the input was empty.
    commentBeforeGap :: Bool,
    -- | Whether anything other than whitespace follows it on its closing
    -- line.
    commentFollowed :: Bool
  }
  deriving (Eq, Show)

-- | Was the comment written after code on its line?
commentTrailing :: Comment -> Bool
commentTrailing = isJust . commentFollows

-- | Does this comment let code follow it on the same line?
closesItself :: Comment -> Bool
closesItself c = commentStyle c == BlockComment && singleLine c

-- | Is this comment a single line?
singleLine :: Comment -> Bool
singleLine c = case commentBody c of
  (_ :| []) -> True
  _ -> False

-- | Every comment in a module, in source order.
commentsOf :: Text -> HsModule GhcPs -> [Comment]
commentsOf source hsModule =
  map (uncurry (mkComment sourceLines))
    . dedupeOnSpan
    . sortOn (GHC.realSrcSpanStart . fst)
    . mapMaybe located
    . concatMap annComments
    $ listify anyAnnComments hsModule
  where
    sourceLines = T.lines source

    -- The tree is walked for annotations rather than for comments, and one
    -- comment can be reachable through more than one annotation, so the
    -- same span can come back twice.
    dedupeOnSpan = \case
      (x : y : rest) | fst x == fst y -> dedupeOnSpan (x : rest)
      (x : rest) -> x : dedupeOnSpan rest
      [] -> []

    anyAnnComments :: GHC.EpAnnComments -> Bool
    anyAnnComments _ = True

    annComments = \case
      GHC.EpaComments xs -> xs
      GHC.EpaCommentsBalanced xs ys -> xs <> ys

    located (GHC.L anchor (GHC.EpaComment tok _)) = case anchor of
      GHC.EpaSpan (GHC.RealSrcSpan s _) -> Just (s, tok)
      _ -> Nothing

-- | Build a comment from a token and the span it occupied.
mkComment :: [Text] -> GHC.RealSrcSpan -> GHC.EpaCommentTok -> Comment
mkComment sourceLines spn tok =
  Comment
    { commentSpan = spanOfReal spn,
      commentBody = normalizeBody startColumn style raw,
      commentStyle = style,
      commentFollows = follows,
      commentAfterGap = afterGap,
      commentBeforeGap = beforeGap,
      commentFollowed = followed
    }
  where
    (style, raw) = case tok of
      GHC.EpaLineComment s -> (LineComment, T.pack s)
      GHC.EpaBlockComment s -> (BlockComment, T.pack s)
      GHC.EpaDocComment _ -> (DocComment, sliceSpan sourceLines spn)
      GHC.EpaDocOptions s -> (LineComment, T.pack s)
    -- Indentation is how many characters precede, which is not the column:
    -- see 'offsetOf'.
    startColumn = maybe 0 (`offsetOf` GHC.srcSpanStartCol spn) openingLine
    openingLine = case drop (GHC.srcSpanStartLine spn - 1) sourceLines of
      (l : _) -> Just l
      [] -> Nothing
    follows = do
      l <- openingLine
      let before' = T.stripEnd (T.take startColumn l)
      if T.null before' then Nothing else Just (columnOf l (T.length before'))
    afterGap = case drop (GHC.srcSpanStartLine spn - 2) sourceLines of
      (l : _) | GHC.srcSpanStartLine spn > 1 -> T.all isSpace l
      _ -> False
    beforeGap = case drop (GHC.srcSpanEndLine spn) sourceLines of
      (l : _) -> T.all isSpace l
      [] -> False
    followed = case drop (GHC.srcSpanEndLine spn - 1) sourceLines of
      (l : _) -> not (T.all isSpace (T.drop (offsetOf l (GHC.srcSpanEndCol spn)) l))
      [] -> False

-- | Apply the normalizations, in the only order that works: dedent before
-- stripping, since a line of nothing but spaces has to still count as
-- indented when the common indentation is measured.
normalizeBody :: Int -> CommentStyle -> Text -> NonEmpty Text
normalizeBody startColumn style raw =
  case NE.nonEmpty (T.lines raw) of
    Nothing -> spaceAfterDashes style raw :| []
    Just (first' :| rest) ->
      fmap T.stripEnd (spaceAfterDashes style first' :| map dedent rest)
  where
    dedent l = T.drop (min startColumn (T.length (T.takeWhile isSpace l))) l

-- | Put a space between a doc comment's trigger and the text after it, so
-- that @-- |Foo@ comes out as @-- | Foo@.
--
-- Only doc comments have triggers; on anything else this is a no-op.
widenTrigger :: Comment -> Comment
widenTrigger c
  | DocComment <- commentStyle c,
    (headLine :| rest) <- commentBody c,
    Just (upToTrigger, body) <- splitTrigger headLine,
    not (T.null body),
    not (" " `T.isPrefixOf` body) =
      c {commentBody = (upToTrigger <> " " <> body) :| map shiftOne rest}
  | otherwise = c
  where
    shiftOne l = case openerWidth l of
      Just n -> let (o, r) = T.splitAt n l in o <> " " <> r
      Nothing -> " " <> l

-- | Put a backslash in front of a doc comment's trigger.
--
-- For a doc comment the compiler did not manage to attach to anything: it
-- is going to come back out as an ordinary comment, and written as it
-- stands it would be lexed as a doc comment again on the next pass, so the
-- formatter would not have a fixed point. The backslash is what Haddock
-- reads as \"this is not a trigger\".
escapeTrigger :: Comment -> Comment
escapeTrigger c = case commentStyle c of
  DocComment ->
    c
      { commentBody = fmap escape (commentBody c),
        commentStyle = ordinaryStyle
      }
  _ -> c
  where
    ordinaryStyle
      | "{-" `T.isPrefixOf` NE.head (commentBody c) = BlockComment
      | otherwise = LineComment

    escape l = case openerWidth l of
      Just n
        | (gap, rest) <- T.span (== ' ') (T.drop n l),
          triggered rest ->
            T.take n l <> (if T.null gap then " " else gap) <> "\\" <> rest
      _ -> l

-- | Is there nothing after this comment's trigger? In that case it is not a
-- Haddock but an ordinary comment.
documentsNothing :: Comment -> Bool
documentsNothing c = case splitTrigger headLine of
  Nothing -> False
  Just (_, body) -> all (T.null . content) (body : rest)
  where
    headLine :| rest = commentBody c
    content = T.strip . without "-}" T.stripSuffix . without "--" T.stripPrefix . T.strip
    without affix strip t = maybe t id (strip affix t)

-- | Has this comment been through 'escapeTrigger'?
--
-- What it was written as cannot be read off the comment any more—that is
-- the point of escaping—so anything wanting to know whether a comment
-- started life as a Haddock has to ask this.
triggerEscaped :: Comment -> Bool
triggerEscaped c = case openerWidth headLine of
  Nothing -> False
  Just n -> case T.uncons (T.dropWhile (== ' ') (T.drop n headLine)) of
    Just ('\\', rest) -> triggered rest
    _ -> False
  where
    headLine = NE.head (commentBody c)

-- | Does this text begin with one of the characters that opens a Haddock?
triggered :: Text -> Bool
triggered t = case T.uncons t of
  Just (ch, _) -> ch `elem` ("|^*$" :: String)
  Nothing -> False

-- | Split a doc comment's opening line into everything up to and including
-- its trigger, and whatever follows.
splitTrigger :: Text -> Maybe (Text, Text)
splitTrigger l = do
  n <- openerWidth l
  let (opener, afterOpener) = T.splitAt n l
      (gap, rest) = T.span (== ' ') afterOpener
  (trigger, body) <- case T.uncons rest of
    Just ('|', b) -> Just ("|", b)
    Just ('^', b) -> Just ("^", b)
    Just ('*', _) -> Just (T.span (== '*') rest)
    _ -> Nothing
  pure (opener <> gap <> trigger, body)

-- | How many characters open a comment, if it opens one.
openerWidth :: Text -> Maybe Int
openerWidth l
  | "--" `T.isPrefixOf` l = Just 2
  | "{-" `T.isPrefixOf` l = Just 2
  | otherwise = Nothing

-- | @--foo@ becomes @-- foo@; @----@ and @-- foo@ are left alone.
--
-- Only the opening line of a line comment is eligible. Inside a block
-- comment a @--@ is just two characters the author wrote.
spaceAfterDashes :: CommentStyle -> Text -> Text
spaceAfterDashes BlockComment t = t
spaceAfterDashes _ t = case T.stripPrefix "--" t of
  Nothing -> t
  Just rest -> case T.uncons rest of
    Nothing -> t
    Just (c, _)
      | c == ' ' || c == '-' -> t
      | otherwise -> "-- " <> rest

-- | Put a comment back together as it will appear in the output.
renderComment :: Comment -> Text
renderComment = T.intercalate "\n" . NE.toList . commentBody

----------------------------------------------------------------------------
-- Pragmas

-- | A compiler pragma, which is written as a block comment but is not one.
--
-- GHC reads pragmas only from the file header, so where a pragma sits
-- decides whether it does anything at all. That is why recognising one is
-- not enough on its own: see 'Tilia.Parser.pmHeaderEnd' for the boundary
-- that says which pragmas are real.
data Pragma = Pragma
  { -- | The name, upper-cased as GHC expects it, e.g. @LANGUAGE@.
    pragmaName :: Text,
    -- | Everything between the name and the closing @#-}@, with the
    -- surrounding whitespace removed but nothing else touched.
    pragmaBody :: Text
  }
  deriving (Eq, Show)

-- | Recognise a pragma.
commentPragma :: Comment -> Maybe Pragma
commentPragma c = do
  inner <- T.stripSuffix "#-}" =<< T.stripPrefix "{-#" oneLine
  let (name, body) = T.break isSpace (T.stripStart inner)
  if T.null name
    then Nothing
    else
      Just
        Pragma
          { pragmaName = T.toUpper name,
            pragmaBody = T.strip body
          }
  where
    oneLine = T.unwords (map T.strip (NE.toList (commentBody c)))

-- | The text a span covers.
sliceSpan :: [Text] -> GHC.RealSrcSpan -> Text
sliceSpan sourceLines spn =
  T.intercalate "\n" (zipWith clip [startLine ..] covered)
  where
    covered =
      take (endLine - startLine + 1) (drop (startLine - 1) sourceLines)
    clip n l =
      (if n == startLine then T.drop (offsetOf l startCol) else id)
        . (if n == endLine then T.take (offsetOf l endCol) else id)
        $ l

    startLine = GHC.srcSpanStartLine spn
    endLine = GHC.srcSpanEndLine spn
    startCol = GHC.srcSpanStartCol spn
    endCol = GHC.srcSpanEndCol spn

----------------------------------------------------------------------------
-- Columns and offsets

-- | How many characters of a line come before the compiler's column.
--
-- A column is not a character offset. The lexer counts a tab as advancing to
-- the next multiple of eight, so a line with a tab in it has more columns
-- than it has characters, and cutting the text at a column would cut in the
-- wrong place. In a file indented with tabs that is every line.
offsetOf :: Text -> Int -> Int
offsetOf line column = T.length (T.take (walk 0 1) line)
  where
    walk i c
      | c >= column = i
      | i >= T.length line = i + (column - c)
      | otherwise = walk (i + 1) (afterChar (T.index line i) c)

-- | The compiler's column for the character at this offset.
columnOf :: Text -> Int -> Int
columnOf line offset = T.foldl' (flip afterChar) 1 (T.take offset line)

-- | Where the column moves to once this character has been read.
afterChar :: Char -> Int -> Int
afterChar ch c
  | ch == '\t' = ((c - 1) `div` 8 + 1) * 8 + 1
  | otherwise = c + 1
