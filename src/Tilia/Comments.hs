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
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs (HsModule)
import GHC.Hs.Extension (GhcPs)
import GHC.Parser.Annotation qualified as GHC
import GHC.Types.SrcLoc qualified as GHC
import Tilia.Printer.Combinators (Span, mkSpan)

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
    -- | Whether anything other than whitespace preceded it on its opening
    -- line. This is what separates a comment trailing some code from one
    -- written on a line of its own, and no amount of looking at the
    -- comment alone can tell the two apart.
    commentTrailing :: Bool
  }
  deriving (Eq, Show)

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
    { commentSpan = toSpan spn,
      commentBody = normalizeBody startColumn style raw,
      commentStyle = style,
      commentTrailing = trailing
    }
  where
    (style, raw) = case tok of
      GHC.EpaLineComment s -> (LineComment, T.pack s)
      GHC.EpaBlockComment s -> (BlockComment, T.pack s)
      GHC.EpaDocComment _ -> (DocComment, sliceSpan sourceLines spn)
      GHC.EpaDocOptions s -> (LineComment, T.pack s)
    -- Columns are 1-based; indentation is how many characters precede.
    startColumn = GHC.srcSpanStartCol spn - 1
    trailing = case drop (GHC.srcSpanStartLine spn - 1) sourceLines of
      (l : _) -> not (T.all isSpace (T.take startColumn l))
      [] -> False

-- | Apply the normalizations, in the only order that works: dedent before
-- stripping, since a line of nothing but spaces has to still count as
-- indented when the common indentation is measured, and widen the trigger
-- last, since it is the one that can add a character.
normalizeBody :: Int -> CommentStyle -> Text -> NonEmpty Text
normalizeBody startColumn style raw =
  case NE.nonEmpty (T.lines raw) of
    Nothing -> spaceAfterDashes style raw :| []
    Just (first' :| rest) ->
      let common = minimum (startColumn : map indentOf rest)
          indentOf l
            | T.all isSpace l = startColumn
            | otherwise = T.length (T.takeWhile isSpace l)
          dedented =
            spaceAfterDashes style first' :| map (T.drop common) rest
       in fmap T.stripEnd (widenTrigger style dedented)

-- | Put a space between a doc comment's trigger and the text after it, so
-- that @-- |Foo@ comes out as @-- | Foo@.
--
-- Only doc comments have triggers; on anything else this is a no-op.
widenTrigger :: CommentStyle -> NonEmpty Text -> NonEmpty Text
widenTrigger DocComment lns@(headLine :| rest) =
  case splitTrigger headLine of
    Just (upToTrigger, body)
      | not (T.null body),
        not (" " `T.isPrefixOf` body) ->
          (upToTrigger <> " " <> body) :| map shiftOne rest
    _ -> lns
  where
    -- The whole comment moves right by one, not just its first line.
    -- Haddock drops a leading space from every line of a doc string when
    -- the first line has one, so widening the first line alone would take
    -- a space away from all the others.
    shiftOne l = case openerWidth l of
      Just n -> let (o, r) = T.splitAt n l in o <> " " <> r
      Nothing -> " " <> l
widenTrigger _ lns = lns

-- | Split a doc comment's opening line into everything up to and including
-- its trigger, and whatever follows.
splitTrigger :: Text -> Maybe (Text, Text)
splitTrigger l = do
  n <- openerWidth l
  let (opener, afterOpener) = T.splitAt n l
      (gap, rest) = T.span (== ' ') afterOpener
  (trigger, body) <- case T.uncons rest of
    -- A named anchor is left alone: the name in @-- $section@ is part of
    -- the anchor, and a space would make it a different one.
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
--
-- Only a single-line block comment can be one: a pragma spread over several
-- lines could not be hoisted without deciding how to re-lay it out, and a
-- pragma is not ours to re-lay out.
commentPragma :: Comment -> Maybe Pragma
commentPragma c = case commentBody c of
  (l :| []) -> do
    inner <- T.stripSuffix "#-}" =<< T.stripPrefix "{-#" l
    let (name, body) = T.break isSpace (T.stripStart inner)
    if T.null name
      then Nothing
      else
        Just
          Pragma
            { pragmaName = T.toUpper name,
              pragmaBody = T.strip body
            }
  _ -> Nothing

-- | The text a span covers.
sliceSpan :: [Text] -> GHC.RealSrcSpan -> Text
sliceSpan sourceLines spn =
  case take (endLine - startLine + 1) (drop (startLine - 1) sourceLines) of
    [] -> ""
    [only] -> T.take (endCol - startCol) (T.drop (startCol - 1) only)
    (first' : rest) ->
      T.intercalate "\n" (T.drop (startCol - 1) first' : trimLast rest)
  where
    startLine = GHC.srcSpanStartLine spn
    endLine = GHC.srcSpanEndLine spn
    startCol = GHC.srcSpanStartCol spn
    endCol = GHC.srcSpanEndCol spn
    trimLast xs = case reverse xs of
      [] -> []
      (y : ys) -> reverse (T.take (endCol - 1) y : ys)

-- | Convert a GHC span to the printer's.
toSpan :: GHC.RealSrcSpan -> Span
toSpan s =
  mkSpan
    (GHC.srcSpanStartLine s, GHC.srcSpanStartCol s)
    (GHC.srcSpanEndLine s, GHC.srcSpanEndCol s)
