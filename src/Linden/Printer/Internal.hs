{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The document representation and the engine that turns it into text.
--
-- Printing code should not import this module; import
-- "Linden.Printer.Combinators" instead, which exposes 'Doc' abstractly
-- along with the vocabulary for building one. This module is for the engine
-- itself and for tests that need to look inside a document.
--
-- The printer is split in two halves that meet at 'Doc'. Code that walks
-- the syntax tree builds a 'Doc', which is an ordinary immutable value with
-- no notion of columns, indentation or what has already been written. The
-- engine in this module is the only thing that knows about those, and it
-- learns them by walking the finished document. Nothing in the first half
-- can observe the second, which is what keeps printing code from having to
-- reason about emission order.
module Linden.Printer.Internal
  ( -- * Source spans
    Span (..),
    mkSpan,
    spanIsSingleLine,

    -- * Documents
    Doc (..),
    Layout (..),
    groupLayout,

    -- * Rendering
    RenderOptions (..),
    defaultRenderOptions,
    render,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

----------------------------------------------------------------------------
-- Source spans

-- | A region of the input.
--
-- Deliberately not GHC's @RealSrcSpan@. The engine needs to ask exactly one
-- question of a span—did it occupy a single line—and defining our own type
-- keeps this module and everything above it free of a dependency on the
-- parser. Conversion happens once, where the syntax tree is walked.
data Span = Span
  { spanStartLine :: !Int,
    spanStartColumn :: !Int,
    spanEndLine :: !Int,
    spanEndColumn :: !Int
  }
  deriving (Eq, Show)

-- | Build a 'Span' from start and end positions, each a line and a column.
mkSpan :: (Int, Int) -> (Int, Int) -> Span
mkSpan (sl, sc) (el, ec) = Span sl sc el ec

-- | Did this span occupy a single line of the input?
spanIsSingleLine :: Span -> Bool
spanIsSingleLine s = spanStartLine s == spanEndLine s

-- | The smallest span covering both arguments.
--
-- Printing code needs this often enough—a construct the syntax tree has no
-- single node for still has to be laid out as a unit—that it is worth
-- having as an instance rather than as a function each caller reimplements.
instance Semigroup Span where
  a <> b =
    Span
      { spanStartLine = min (spanStartLine a) (spanStartLine b),
        spanStartColumn = case compare (spanStartLine a) (spanStartLine b) of
          LT -> spanStartColumn a
          GT -> spanStartColumn b
          EQ -> min (spanStartColumn a) (spanStartColumn b),
        spanEndLine = max (spanEndLine a) (spanEndLine b),
        spanEndColumn = case compare (spanEndLine a) (spanEndLine b) of
          GT -> spanEndColumn a
          LT -> spanEndColumn b
          EQ -> max (spanEndColumn a) (spanEndColumn b)
      }

----------------------------------------------------------------------------
-- Documents

-- | A description of what to print.
data Doc
  = -- | Print nothing.
    DEmpty
  | -- | A literal fragment. Must not contain a line break: the engine
    -- tracks columns by counting characters, and an embedded newline would
    -- make that count wrong. Use 'DHardBreak'.
    DText !Text
  | -- | A space. Repeats collapse, and one at the end of a line is dropped,
    -- so printing code may emit them freely rather than working out whether
    -- one is already there.
    DSpace
  | -- | A space when the enclosing group is flat, a line break when it is
    -- broken.
    DBreak
  | -- | Nothing when the enclosing group is flat, a line break when it is
    -- broken.
    DSoftBreak
  | -- | A line break regardless of the enclosing group.
    --
    -- Two in a row leave one empty line between what surrounds them, which
    -- is all a blank line is; there is deliberately no separate constructor
    -- for one. Further breaks add nothing.
    DHardBreak
  | -- | Concatenation. See the 'Semigroup' instance.
    DCat !Doc !Doc
  | -- | Indent the enclosed document by the given number of steps, relative
    -- to the current indentation.
    DNest !Int !Doc
  | -- | Indent the enclosed document to whatever column the line has
    -- reached, so that it lines up under itself when broken.
    DAlign !Doc
  | -- | Lay the enclosed document out flat or broken.
    --
    -- The decision is already made by the time it reaches the engine.
    -- 'groupLayout' is what makes it, from the span the construct occupied
    -- in the input, and it lives in the combinator layer's vocabulary
    -- rather than here so that the engine has no policy in it at all.
    DGroup !Layout !Doc
  | -- | Choose between two documents according to the enclosing group: the
    -- first when it is flat, the second when it is broken.
    --
    -- For the constructs whose two layouts differ by more than where the
    -- breaks fall. Asking which layout is in force is the one question
    -- printing code genuinely cannot answer for itself, and answering it
    -- with a constructor rather than by handing printing code a monad is
    -- what keeps that code free of emission order.
    DVariant !Doc !Doc
  | -- | Record that the enclosed document was produced from the given
    -- region of the input.
    --
    -- This carries no layout meaning at all and the engine ignores it.
    -- Keeping provenance separate from grouping is deliberate: the two
    -- coincide often, but a construct can need one without the other, and
    -- fusing them is what forces a printer to grow an escape hatch for each
    -- case where they come apart.
    DLocated !Span !Doc
  deriving (Eq, Show)

-- | Documents concatenate. @'DEmpty'@ is the unit, so a document is a
-- monoid and printing code can use @'mconcat'@, @'foldMap'@ and the rest of
-- the ordinary vocabulary instead of a bespoke sequencing operator.
instance Semigroup Doc where
  DEmpty <> b = b
  a <> DEmpty = a
  a <> b = DCat a b

instance Monoid Doc where
  mempty = DEmpty

-- | Whether a group is laid out on one line or across several.
data Layout
  = Flat
  | Broken
  deriving (Eq, Show)

-- | Decide how to lay a group out.
--
-- This is the whole of the policy, in one place on purpose. Layout follows
-- the input: a construct written on one line stays on one line, and one
-- that was spread out stays spread out. A group with no span is one the
-- printer synthesised rather than read, and has nothing to follow, so it
-- goes flat.
--
-- Notably absent is any notion of a maximum line width. Nothing in the
-- engine measures the result against a limit, so a long line that was
-- written as one line is reproduced as one line.
groupLayout :: Maybe Span -> Layout
groupLayout = \case
  Nothing -> Flat
  Just s
    | spanIsSingleLine s -> Flat
    | otherwise -> Broken

----------------------------------------------------------------------------
-- Rendering

-- | Knobs for 'render'.
newtype RenderOptions = RenderOptions
  { -- | Columns per indentation step.
    roIndentStep :: Int
  }
  deriving (Eq, Show)

-- | Two columns per step.
defaultRenderOptions :: RenderOptions
defaultRenderOptions = RenderOptions {roIndentStep = 2}

-- | What the engine carries while walking a document.
--
-- Indentation and layout flow downwards and are restored on the way out, so
-- they are passed as arguments. Everything else is output being
-- accumulated.
data Env = Env
  { envIndent :: !Int,
    envLayout :: !Layout,
    envIndentStep :: !Int
  }

-- | Output built so far.
--
-- Lines are finished one at a time and never revisited, so the current line
-- is kept as a reversed list of fragments and completed lines as a reversed
-- list of lines.
data Out = Out
  { -- | Completed lines, most recent first.
    outLines :: [Text],
    -- | Fragments of the line being built, most recent first.
    outCurrent :: [Text],
    -- | Column the current line has reached.
    outColumn :: !Int,
    -- | Whether anything has been written to the current line. Indentation
    -- is emitted lazily, when the first fragment arrives, so that a line
    -- with nothing on it stays genuinely empty.
    outStarted :: !Bool
  }

emptyOut :: Out
emptyOut =
  Out
    { outLines = [],
      outCurrent = [],
      outColumn = 0,
      outStarted = False
    }

-- | Turn a document into text.
render :: RenderOptions -> Doc -> Text
render opts doc = finish (go env doc emptyOut)
  where
    env =
      Env
        { envIndent = 0,
          envLayout = Broken,
          envIndentStep = roIndentStep opts
        }

-- | Walk a document, accumulating output.
go :: Env -> Doc -> Out -> Out
go env = \case
  DEmpty -> id
  DText t -> putText (envIndent env) t
  DSpace -> putSpace
  DBreak -> case envLayout env of
    Flat -> putSpace
    Broken -> breakLine
  DSoftBreak -> case envLayout env of
    Flat -> id
    Broken -> breakLine
  DHardBreak -> breakLine
  DCat a b -> go env b . go env a
  DNest n d -> go env {envIndent = envIndent env + n * envIndentStep env} d
  DAlign d -> \out ->
    go env {envIndent = max (envIndent env) (outColumn out)} d out
  DGroup l d -> go env {envLayout = l} d
  DVariant flatD brokenD -> case envLayout env of
    Flat -> go env flatD
    Broken -> go env brokenD
  DLocated _ d -> go env d

-- | Append a fragment, emitting the line's indentation first if this is the
-- first thing on it.
putText :: Int -> Text -> Out -> Out
putText indent t out
  | T.null t = out
  | outStarted out =
      out
        { outCurrent = t : outCurrent out,
          outColumn = outColumn out + T.length t
        }
  | otherwise =
      out
        { outCurrent = [t, T.replicate indent " "],
          outColumn = indent + T.length t,
          outStarted = True
        }

-- | Append a space, unless the line has not started or already ends in one.
putSpace :: Out -> Out
putSpace out
  | not (outStarted out) = out
  | endsWithSpace out = out
  | otherwise =
      out
        { outCurrent = " " : outCurrent out,
          outColumn = outColumn out + 1
        }

endsWithSpace :: Out -> Bool
endsWithSpace out = case outCurrent out of
  (t : _) -> maybe False ((== ' ') . snd) (T.unsnoc t)
  [] -> False

-- | Finish the current line.
--
-- Two breaks in a row leave one empty line between the text either side of
-- them, which is a blank line the author asked for. Further breaks add
-- nothing: the output never carries two blank lines in a row, however many
-- times printing code breaks. Breaking before anything has been written is
-- dropped for the same reason, since 'finish' strips empty lines only from
-- the end.
--
-- Between them these two rules mean printing code may break wherever a
-- break might be wanted without first working out what it already emitted.
breakLine :: Out -> Out
breakLine out
  | atStart out = out
  | wouldRepeatBlank out =
      out {outCurrent = [], outColumn = 0, outStarted = False}
  | otherwise =
      out
        { outLines = currentLine out : outLines out,
          outCurrent = [],
          outColumn = 0,
          outStarted = False
        }

-- | Would finishing this line put a second empty line in a row?
wouldRepeatBlank :: Out -> Bool
wouldRepeatBlank out = case (outStarted out, outLines out) of
  (False, "" : _) -> True
  _ -> False

-- | Is the output still empty?
atStart :: Out -> Bool
atStart out = null (outLines out) && not (outStarted out)

-- | The current line, with trailing whitespace removed.
--
-- Stripping here, once, is why nothing upstream has to avoid emitting a
-- space before a line break.
currentLine :: Out -> Text
currentLine = T.stripEnd . T.concat . reverse . outCurrent

-- | Assemble the final text: one trailing newline, no blank lines at the
-- end, no trailing whitespace anywhere.
finish :: Out -> Text
finish out =
  case dropWhile T.null (outLines (breakLine out)) of
    [] -> ""
    ls -> T.unlines (reverse ls)
