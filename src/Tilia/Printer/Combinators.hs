{-# LANGUAGE OverloadedStrings #-}

-- | The vocabulary for writing printing code.
module Tilia.Printer.Combinators
  ( -- * Documents
    Doc,
    Span (..),
    mkSpan,

    -- * Atoms
    txt,
    space,
    breakOrSpace,
    breakOrNothing,
    hardBreak,
    blankLine,
    emptyAnchor,

    -- * Layout
    Layout (..),
    group,
    flat,
    broken,
    variant,
    located,

    -- * Attachment
    Placement (..),
    attach,
    hangingIfSingleLine,

    -- * Indentation
    nest,
    indent,
    align,

    -- * Combining
    hcat,
    hsep,
    vsep,
    sepBy,
    punctuate,

    -- * Wrapping
    enclose,
    bracket,
    parens,
    brackets,
    braces,
    banana,
    unboxed,
    backticks,

    -- * Punctuation
    comma,
    commaSep,
    semi,

    -- * Conditionals
    includeWhen,
    includeUnless,
  )
where

import Data.List (intersperse)
import Data.Text (Text)
import Tilia.Printer.Internal
  ( Doc (..),
    Layout (..),
    Span (..),
    groupLayout,
    mkSpan,
    spanIsSingleLine,
  )

----------------------------------------------------------------------------
-- Atoms

-- | A literal fragment of output.
--
-- The argument must not contain a line break; use 'hardBreak'. This is for
-- keywords, punctuation and names—anything whose spelling is fixed.
txt :: Text -> Doc
txt = DText

-- | A space. Repeated spaces collapse and a space before a line break is
-- dropped.
space :: Doc
space = DSpace

-- | A place the line may break. It becomes a line break if the enclosing
-- 'group' is broken, and a space if it is flat. This is the workhorse: it
-- is what lets one printer serve both layouts.
breakOrSpace :: Doc
breakOrSpace = DBreak

-- | A place the line may break, leaving nothing behind if it does not. For
-- the positions where the two layouts differ by a break rather than by a
-- space, such as immediately inside a bracket.
breakOrNothing :: Doc
breakOrNothing = DSoftBreak

-- | A line break, whatever the enclosing group decided.
hardBreak :: Doc
hardBreak = DHardBreak

-- | An empty line.
blankLine :: Doc
blankLine = hardBreak <> hardBreak

-- | An anchor for a construct that contains nothing.
emptyAnchor :: Span -> Doc
emptyAnchor s = located s mempty

----------------------------------------------------------------------------
-- Layout

-- | Lay the document out as the input had it: flat if the construct was
-- written on one line, broken if it was spread across several.
group :: Span -> Doc -> Doc
group s = DGroup (groupLayout (Just s))

-- | Force flat layout.
flat :: Doc -> Doc
flat = DGroup Flat

-- | Force broken layout.
broken :: Doc -> Doc
broken = DGroup Broken

-- | Choose according to the layout the enclosing 'group' settled on.
--
-- Reach for this only when the two layouts differ by more than where the
-- breaks fall; when they differ only in that, 'breakOrSpace' and
-- 'breakOrNothing' already say so and read better.
variant ::
  -- | When flat
  Doc ->
  -- | When broken
  Doc ->
  Doc
variant = DVariant

-- | Record where the output is coming from in the input.
--
-- This has no effect on layout. It is provenance, kept so that later
-- passes—comment attachment above all—can ask which region of the input a
-- piece of the document corresponds to.
located :: Span -> Doc -> Doc
located = DLocated

----------------------------------------------------------------------------
-- Attachment

-- | Whether a construct absorbs its own line break.
data Placement
  = -- | The preceding construct breaks and indents.
    Normal
  | -- | The construct is handed the rest of the line and breaks itself.
    Hanging
  deriving (Eq, Show)

-- | Join a body to whatever precedes it, according to its 'Placement'.
attach :: Placement -> Doc -> Doc
attach Hanging body = space <> body
attach Normal body = breakOrSpace <> indent body

-- | 'Hanging' if the span was a single line in the input, 'Normal'
-- otherwise.
--
-- A handful of constructs hang only when what comes before their own break
-- was written on one line—a lambda whose parameters ran on, for instance,
-- would leave the body indented under nothing legible. Those constructs
-- consult the input, exactly as 'group' does, and this is the shared
-- spelling of that question so that it reads as policy rather than as a
-- special case repeated in each classifier.
hangingIfSingleLine :: Span -> Placement
hangingIfSingleLine s = if spanIsSingleLine s then Hanging else Normal

----------------------------------------------------------------------------
-- Indentation

-- | Indent by the given number of steps, relative to the current level.
nest :: Int -> Doc -> Doc
nest = DNest

-- | Indent by one step.
indent :: Doc -> Doc
indent = DNest 1

-- | Indent to the column the line has already reached, so that a broken
-- construct lines up under its own beginning rather than under the start of
-- the line.
align :: Doc -> Doc
align = DAlign

----------------------------------------------------------------------------
-- Combining

-- | Concatenate, with nothing in between.
hcat :: [Doc] -> Doc
hcat = mconcat

-- | Concatenate, separated by 'space'.
hsep :: [Doc] -> Doc
hsep = sepBy space

-- | Concatenate, separated by 'hardBreak'.
vsep :: [Doc] -> Doc
vsep = sepBy hardBreak

-- | Concatenate, separated by the given document.
sepBy :: Doc -> [Doc] -> Doc
sepBy s = mconcat . intersperse s

-- | Append the separator to every element but the last.
--
-- For the cases where the separator has to travel with the element rather
-- than sit between elements, such as a trailing comma that must stay on the
-- line above a break.
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate _ [x] = [x]
punctuate s (x : xs) = (x <> s) : punctuate s xs

----------------------------------------------------------------------------
-- Wrapping

-- | Surround with the given opening and closing documents, adding nothing
-- of its own.
enclose ::
  -- | Opening
  Doc ->
  -- | Closing
  Doc ->
  -- | Body
  Doc ->
  Doc
enclose open close body = open <> body <> close

-- | Surround with a bracket pair that opens up when broken.
--
-- Flat, this is @open body close@ with nothing added. Broken, the body
-- moves to its own indented lines and the closing bracket goes back out to
-- the opening bracket's level.
bracket ::
  -- | Opening
  Text ->
  -- | Closing
  Text ->
  -- | Body
  Doc ->
  Doc
bracket open close body =
  txt open <> indent (breakOrNothing <> body) <> breakOrNothing <> txt close

-- | @(@ and @)@.
parens :: Doc -> Doc
parens = bracket "(" ")"

-- | @[@ and @]@.
brackets :: Doc -> Doc
brackets = bracket "[" "]"

-- | @{@ and @}@.
braces :: Doc -> Doc
braces = bracket "{" "}"

-- | @(|@ and @|)@, from arrow notation.
banana :: Doc -> Doc
banana = bracket "(|" "|)"

-- | @(#@ and @#)@, for unboxed tuples and sums.
unboxed :: Doc -> Doc
unboxed body = txt "(#" <> space <> body <> space <> txt "#)"

-- | Surround with backticks.
backticks :: Doc -> Doc
backticks = enclose (txt "`") (txt "`")

----------------------------------------------------------------------------
-- Punctuation

-- | @,@.
comma :: Doc
comma = txt ","

-- | @;@.
semi :: Doc
semi = txt ";"

-- | Separate by a comma and a 'breakOrSpace', so that a broken list puts each
-- element on its own line with the comma left behind on the one above.
commaSep :: [Doc] -> Doc
commaSep = sepBy (comma <> breakOrSpace)

----------------------------------------------------------------------------
-- Conditionals

-- | The document if the condition holds, nothing otherwise.
includeWhen :: Bool -> Doc -> Doc
includeWhen b d = if b then d else mempty

-- | The document unless the condition holds.
includeUnless :: Bool -> Doc -> Doc
includeUnless b = includeWhen (not b)
