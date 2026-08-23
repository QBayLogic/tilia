{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The document representation and the engine that turns it into text.
--
-- Printing code should not import this module; import
-- "Tilia.Doc.Combinators" instead, which exposes 'Doc' abstractly
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
module Tilia.Doc.Internal
  ( -- * Documents
    Doc (..),
    Layout (..),
    Resume (..),
    groupLayout,

    -- * Rendering
    RenderOptions (..),
    defaultRenderOptions,
    render,
  )
where

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Tilia.Span (Span, isSingleLine)

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
  | -- | Text to be put at the end of the line this position falls on,
    -- however much of the line is still to be written.
    --
    -- For a comment the author wrote at the end of a line. Where it belongs
    -- is not a position in the document but a position in the /output/: it
    -- has to come after everything else on its line, including punctuation
    -- the printer has not emitted yet. Putting it in the document where the
    -- node it trails happens to sit would push a comma, an arrow or a
    -- closing bracket onto the next line.
    --
    -- One line holds one of these. Two would mean two comments trailing
    -- what turned out to be a single line of output, and the engine closes
    -- the line rather than running them together into a comment neither
    -- author wrote.
    DHoldBack !Text
  | -- | Close the line, and let a break that immediately follows know that
    -- it has nothing left to do.
    --
    -- A comment owns the rest of its line, so something has to end that
    -- line; but whatever the comment was attached to very often ends it
    -- too, and two breaks in a row are a blank line. This is the break that
    -- says \"the line is finished\" rather than \"break here\", so the two
    -- do not add up to an empty line nobody asked for.
    DCloseLine
  | -- | A line break between two lines of text that is being reproduced
    -- rather than laid out.
    --
    -- Collapsing nothing and skipping nothing, unlike every other break
    -- here: the lines either side are the author's, so an empty one among
    -- them is content and not spacing. Where the next line begins is the
    -- only thing left to decide, and 'Resume' decides it.
    DVerbatimBreak !Resume
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
    -- first when it is flat, the second when it is broken. For the
    -- constructs whose two layouts differ by more than where the breaks
    -- fall.
    --
    -- Both fields are lazy, and that is not an oversight. The engine walks
    -- one of them and never looks at the other, so the branch not taken
    -- should cost nothing. Were they strict, building a variant would build
    -- both layouts of everything inside it; a construct nested @n@ deep
    -- would be built @2^n@ times.
    DVariant Doc Doc
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

-- | Where the line after a 'DVerbatimBreak' begins.
data Resume
  = -- | At the indentation in force, as any other break would.
    AtIndent
  | -- | At column zero, whatever the indentation.
    AtMargin
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
    | isSingleLine s -> Flat
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
    outStarted :: !Bool,
    -- | Fragments held back until the line ends, in the order they were
    -- given.
    outHeldBack :: !(Maybe Text),
    -- | Whether the line was closed by something that already knew it was
    -- ending it, so that a break arriving now would add an empty line rather
    -- than end anything.
    outClosed :: !Bool
  }

emptyOut :: Out
emptyOut =
  Out
    { outLines = [],
      outCurrent = [],
      outColumn = 0,
      outStarted = False,
      outHeldBack = Nothing,
      outClosed = False
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
  DHoldBack t -> putHeldBack (envIndent env) t
  DCloseLine -> closeLine
  DHardBreak -> breakLine
  DVerbatimBreak resume -> verbatimBreakLine resume
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
putText indent t out0
  | T.null t = out0
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
  where
    out = out0 {outClosed = False}

-- | Hold a fragment back until the line ends.
putHeldBack :: Int -> Text -> Out -> Out
putHeldBack indent t out
  | isJust (outHeldBack out) = putHeldBack indent t (closeLine out)
  | outStarted out = out {outHeldBack = Just t, outClosed = False}
  | otherwise = closeLine (putText indent t out)

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

-- | Close the current line, if there is anything on it.
--
-- Unlike 'breakLine' this leaves a mark: the next break sees that the line
-- was already ended on purpose and does nothing, so a comment that ends its
-- own line and a construct that would have ended it anyway do not between
-- them leave an empty one.
closeLine :: Out -> Out
closeLine out
  | hasContent out = (breakLine out) {outClosed = True}
  | otherwise = out

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
  | outClosed out = out {outClosed = False}
  | atStart out = out
  | wouldRepeatBlank out =
      out {outCurrent = [], outColumn = 0, outStarted = False, outHeldBack = Nothing}
  | otherwise =
      out
        { outLines = currentLine out : outLines out,
          outCurrent = [],
          outColumn = 0,
          outStarted = False,
          outHeldBack = Nothing
        }

-- | Finish the current line between two lines of reproduced text.
verbatimBreakLine :: Resume -> Out -> Out
verbatimBreakLine resume out =
  out
    { outLines = currentLine out : outLines out,
      outCurrent = [],
      outColumn = 0,
      outStarted = resume == AtMargin,
      outHeldBack = Nothing,
      outClosed = False
    }

-- | Would finishing this line put a second empty line in a row?
wouldRepeatBlank :: Out -> Bool
wouldRepeatBlank out = case (hasContent out, outLines out) of
  (False, "" : _) -> True
  _ -> False

-- | Is the output still empty?
atStart :: Out -> Bool
atStart out = null (outLines out) && not (hasContent out)

-- | Is there anything on the current line, written or held back?
hasContent :: Out -> Bool
hasContent out = outStarted out || isJust (outHeldBack out)

-- | The current line: what was written to it, then whatever was held back
-- for its end, with one space between them and no trailing whitespace.
currentLine :: Out -> Text
currentLine out
  | T.null written = heldBack
  | T.null heldBack = written
  | otherwise = written <> " " <> heldBack
  where
    written = T.stripEnd (T.concat (reverse (outCurrent out)))
    heldBack = maybe "" T.stripEnd (outHeldBack out)

-- | Assemble the final text: one trailing newline, no blank lines at the
-- end, no trailing whitespace anywhere.
finish :: Out -> Text
finish out =
  case dropWhile T.null (outLines (breakLine out)) of
    [] -> ""
    ls -> T.unlines (reverse ls)
