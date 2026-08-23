{-# LANGUAGE LambdaCase #-}

-- | What the syntax walk needs to know that the syntax tree does not say.
--
-- Printing a Haskell module is very nearly a fold over its syntax tree, but
-- not quite: a handful of decisions need facts from outside the node being
-- printed. Which extensions are on decides whether @(#foo)@ needs spaces
-- inside its parentheses; which operators are in scope decides how a chain
-- of them may be regrouped; where the comments are decides which constructs
-- may be put on one line.
--
-- The record also carries the knot ('Knot'). Rendering is mutually
-- recursive—a type may contain a splice, which contains an expression,
-- which contains declarations, which contain types—and rather than break
-- the cycle with @hs-boot@ files the few backward edges are held in this
-- record and tied once, in "Tilia.Render". That is what lets the modules
-- below be ordered by syntactic category instead of by what happens to
-- import what.
module Tilia.Render.Context
  ( -- * The context
    Ctx (..),
    Knot (..),
    SourceType (..),
    FamilyStyle (..),

    -- * Where a node stands
    Site (..),
    plainSite,
    withBracing,
    underSite,
    closingFor,

    -- * Extensions
    extensionOn,

    -- * Fixities
    operatorFixity,

    -- * Spans
    commentBetween,
    separatedByBlank,

    -- * Entering the tree
    at,
    at_,
    atSpan,
    layoutFrom,
    layoutAcross,
    insideBrackets,

    -- * Haddocks
    writtenHaddock,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs hiding (Fixity)
import GHC.LanguageExtensions.Type (Extension)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName (..), rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..))
import GHC.Types.SrcLoc qualified as GHC
import Tilia.Comments (Comment (..), CommentStyle (..))
import Tilia.Fixity
  ( Fixity,
    OpName (..),
    Resolution (..),
    Scope,
    lookupFixity,
  )
import Tilia.Span
import Tilia.Span.Ghc
import Tilia.Doc.Combinators
import Tilia.Render.Layout (Bracing (..))

----------------------------------------------------------------------------
-- The context

-- | Whether the file being printed is a module or a Backpack signature.
--
-- The two differ in one word and in one layout rule, which is not enough to
-- justify two printers but is enough that the printer has to be told.
data SourceType
  = ModuleSource
  | SignatureSource
  deriving (Eq, Show)

-- | Whether a family or data declaration stands on its own or inside a
-- class.
--
-- An associated declaration drops the @family@ and @instance@ keywords its
-- free-standing counterpart needs, which is the only thing the printer has
-- to know about the difference.
data FamilyStyle
  = Associated
  | Free
  deriving (Eq, Show)

-- | The backward edges of the rendering knot.
--
-- Each field is a printer defined in a module that the module needing it
-- comes before. Nothing else belongs here: a forward edge is an ordinary
-- import and should stay one.
data Knot = Knot
  { -- | Expressions, needed by types, patterns and bodies.
    knotExpr :: Ctx -> Site -> LHsExpr GhcPs -> Doc,
    -- | Commands, needed by bodies.
    knotCmd :: Ctx -> Site -> LHsCmd GhcPs -> Doc,
    -- | Splices, needed by types and patterns, defined with expressions.
    knotSplice :: Ctx -> SpliceDecoration -> HsUntypedSplice GhcPs -> Doc,
    -- | Signature declarations, needed by @let@ and @where@ bodies.
    knotSig :: Ctx -> Sig GhcPs -> Doc,
    -- | A run of declarations, needed by Template Haskell brackets.
    knotDecls :: Ctx -> FamilyStyle -> [LHsDecl GhcPs] -> Doc,
    -- | A run of declarations that keeps the author's blank lines, needed by
    -- class and instance bodies.
    knotDeclsGrouped :: Ctx -> FamilyStyle -> [LHsDecl GhcPs] -> Doc
  }

-- | Everything a printer may need beyond the node it is given.
data Ctx = Ctx
  { -- | Extensions in force.
    ctxExtensions :: Set Extension,
    -- | Module or signature.
    ctxSourceType :: SourceType,
    -- | What the module can see, if that could be worked out.
    --
    -- 'Nothing' is not the same as an empty scope: it means no answer was
    -- established, and an operator chain whose fixities are unknown is left
    -- exactly as the author arranged it. See "Tilia.Fixity".
    ctxScope :: Maybe Scope,
    -- | Where the comments that take whole lines are, by starting position.
    --
    -- Only these are here, because only these bear on layout: a construct
    -- with one written inside it cannot be put on one line, since the
    -- comment would swallow whatever followed it.
    ctxLineComments :: Map (Int, Int) Span,
    -- | The author's own text for each Haddock, by starting position.
    ctxHaddocks :: Map (Int, Int) Comment,
    -- | The knot.
    ctxKnot :: Knot
  }

----------------------------------------------------------------------------
-- Extensions

-- | Is the extension on?
extensionOn :: Ctx -> Extension -> Bool
extensionOn ctx e = Set.member e (ctxExtensions ctx)

----------------------------------------------------------------------------
-- Fixities

-- | The fixity of an operator, if one was established.
--
-- 'Nothing' means the question was not answered, and the caller must not
-- rearrange anything on the strength of it.
operatorFixity :: Ctx -> RdrName -> Maybe Fixity
operatorFixity ctx name = do
  scope <- ctxScope ctx
  case lookupFixity scope qualifier op of
    Resolved fixity _ -> Just fixity
    Unresolved _ -> Nothing
  where
    op = OpName (T.pack (occNameString (rdrNameOcc name)))
    qualifier = case name of
      Qual m _ -> Just (T.pack (moduleNameString m))
      _ -> Nothing

----------------------------------------------------------------------------
-- Where a node stands

-- | What the surroundings of a node oblige it to do.
--
-- None of this can be read off the node itself, and all of it changes how
-- the node is laid out, which is why it travels alongside. It lives here
-- rather than with the expression printer because the knot has to mention
-- it: a body is a node together with the site it stands at, and the printers
-- that turn one into a document are reached through the knot.
data Site = Site
  { -- | Is this the function of an application, as @f@ is in @f a@?
    siteApplicand :: Bool,
    -- | Is this an item of a layout block?
    --
    -- Such an item may not leave a bracket open for the block's own layout
    -- to close, so a list comprehension standing as a statement is arranged
    -- differently from one standing anywhere else.
    siteInBlock :: Bool,
    -- | May a block inside this node put braces round itself?
    siteBracing :: Bracing
  }
  deriving (Eq, Show)

-- | A node standing on its own.
plainSite :: Site
plainSite =
  Site
    { siteApplicand = False,
      siteInBlock = False,
      siteBracing = NoBrace
    }

-- | The same site, with a different answer about braces.
withBracing :: Bracing -> Site -> Site
withBracing bracing site = site {siteBracing = bracing}

-- | Indent a hanging body, one step further when it hangs off an applicand.
underSite :: Site -> Doc -> Doc
underSite site = nest (if siteApplicand site then 2 else 1)

-- | Where a bracket opened here has to close.
closingFor :: Site -> ClosingIndent
closingFor site = if siteInBlock site then Indented else Outdented

----------------------------------------------------------------------------
-- Spans

-- | Is there a comment taking whole lines between the two?
--
-- What this is really asking is whether something is going to be printed
-- between them that ends a line. An operator with a comment in front of it
-- cannot be moved to the end of the line above, because the comment would go
-- with it and the operand would be left stranded.
commentBetween :: Ctx -> Maybe Span -> Maybe Span -> Bool
commentBetween ctx (Just a) (Just b) =
  case Map.lookupGE (spanEndLine a, spanEndColumn a) (ctxLineComments ctx) of
    Just (start, _) -> start < (spanStartLine b, spanStartColumn b)
    Nothing -> False
commentBetween _ _ _ = False

-- | Did the author leave an empty line directly after the first of these?
--
-- Unlike 'blankBetween' this counts what is going to be printed in the gap
-- rather than only what the two spans say. A comment written between two
-- bindings is printed above the second of them, so it is the comment that
-- the blank line belongs in front of, and measuring to the binding instead
-- would move the blank line past it.
separatedByBlank :: Ctx -> Maybe Span -> Maybe Span -> Bool
separatedByBlank ctx (Just a) (Just b) =
  nextLine > spanEndLine a + 1
  where
    nextLine = maybe (spanStartLine b) spanStartLine (firstCommentBetween ctx a b)
separatedByBlank _ _ _ = False

-- | The first comment written in the gap between two spans.
firstCommentBetween :: Ctx -> Span -> Span -> Maybe Span
firstCommentBetween ctx a b =
  case Map.lookupGE (spanEndLine a, spanEndColumn a) (ctxLineComments ctx) of
    Just (start, s) | start < (spanStartLine b, spanStartColumn b) -> Just s
    _ -> Nothing

----------------------------------------------------------------------------
-- Entering the tree

-- | Enter a located node.
--
-- This is the counterpart of every @L@ in the syntax tree: it records where
-- the output came from, so that comments can be attached to it later, and
-- it settles the node's layout from the region it occupied. A printer that
-- pattern-matches through a located wrapper without going through here has
-- dropped a comment's only anchor.
at :: (HasLoc l) => Ctx -> GenLocated l a -> (a -> Doc) -> Doc
at ctx l f = atSpan ctx (spanOf l) (f (GHC.unLoc l))

-- | 'at' with the arguments the other way round, for use in sections.
at_ :: (HasLoc l) => Ctx -> (a -> Doc) -> GenLocated l a -> Doc
at_ ctx f l = at ctx l f

-- | Lay a region out as it was written, and claim it.
--
-- Claiming is the difference between this and 'layoutFrom': a comment
-- written anywhere inside the region attaches to this document. So it is
-- for the handful of things a comment can be written against that the
-- syntax tree gives no node for—the @where@ that opens a body, the @then@
-- of an @if@—and for nothing else. Claiming a region merely because its
-- layout is being decided would hand every comment inside it to whatever
-- happens to be printed first.
atSpan :: Ctx -> Maybe Span -> Doc -> Doc
atSpan _ Nothing d = d
atSpan ctx (Just s) d = located s (grouped ctx s d)

-- | Lay a region out as it was written, and claim nothing.
--
-- The region decides one thing—whether what is printed here goes on one
-- line or several—and says nothing about where the output came from. That
-- is the whole difference from 'atSpan', and it is why this is the one to
-- reach for by default.
--
-- Given no span at all it lays the document out flat, which is what a
-- construct the printer synthesised rather than read deserves.
layoutFrom :: Ctx -> Maybe Span -> Doc -> Doc
layoutFrom _ Nothing d = flat d
layoutFrom ctx (Just s) d = grouped ctx s d

-- | 'layoutFrom' over the region several located things cover.
layoutAcross :: (HasLoc l) => Ctx -> [GenLocated l a] -> Doc -> Doc
layoutAcross ctx xs = layoutFrom ctx (spansOf xs)

-- | Give the inside of a bracketed construct an anchor at its far end.
--
-- The last element of a list is not the last thing inside its brackets: a
-- comment may be written after it and before the closing bracket, and it
-- belongs inside. Nothing in the syntax tree stands there, so a zero-width
-- anchor at the construct's own end is put there for such a comment to
-- attach to—and it is the only thing a comment written between the brackets
-- of an /empty/ construct has to attach to at all.
--
-- Without it those comments have nothing to hold them and are emitted after
-- the closing bracket, which moves them out of the construct they were
-- written in.
insideBrackets :: Maybe Span -> Doc -> Doc
insideBrackets here d = d <> foldMap (emptyAnchor . endOf) here

-- | Lay a document out according to a span, and to the comments inside it.
--
-- Layout follows the input, except that a comment taking a whole line
-- overrules it. Such a comment owns the rest of its line, so a construct
-- holding one cannot be put on one line however the author wrote it; the
-- closing bracket would end up commented out.
grouped :: Ctx -> Span -> Doc -> Doc
grouped ctx s d
  | holdsLineComment ctx s = broken d
  | otherwise = group s d

-- | Does a comment that takes whole lines begin inside this span?
holdsLineComment :: Ctx -> Span -> Bool
holdsLineComment ctx s =
  case Map.lookupGE (spanStartLine s, spanStartColumn s) (ctxLineComments ctx) of
    Just (start, _) -> start <= (spanEndLine s, spanEndColumn s)
    Nothing -> False

----------------------------------------------------------------------------
-- Haddocks

-- | The author's own text for the Haddock at this position, if we kept it.
--
-- Rebuilding a Haddock from the doc string the syntax tree carries cannot
-- reproduce a @{- | … -}@ or an empty @-- |@, so the text is taken from the
-- comment stream whenever the Haddock is going to come back out in the style
-- it went in as. Deciding that is the caller's business; all this does is
-- find the text.
writtenHaddock :: Ctx -> Maybe Span -> Maybe (NonEmpty Text)
writtenHaddock ctx ms = do
  s <- ms
  c <- Map.lookup (spanStartLine s, spanStartColumn s) (ctxHaddocks ctx)
  case commentStyle c of
    DocComment -> Just (commentBody c)
    _ -> Nothing
