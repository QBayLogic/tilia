{-# LANGUAGE OverloadedStrings #-}

-- | Turning a parsed module into a document.
module Tilia.Render
  ( -- * Settings
    Settings (..),
    defaultSettings,

    -- * Rendering
    renderModule,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Hs (HsModule (..))
import GHC.Hs.Extension (GhcPs)
import GHC.LanguageExtensions.Type (Extension (..))
import Tilia.Comments
  ( Comment (..),
    closesItself,
    documentsNothing,
    escapeTrigger,
    widenTrigger,
  )
import Tilia.Comments.Attach (attachComments)
import Tilia.Fixity (Scope)
import Tilia.Imports (normalizeImports)
import Tilia.Parser (ParsedModule (..))
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Render.Declaration (decls, declsKeepingGroups)
import Tilia.Render.Expression (hsCmd, hsExprIn, untypedSplice)
import Tilia.Render.Haddock (haddockSpans)
import Tilia.Render.Header (hsModule, takeHeaderPragmas, takeStackHeader)
import Tilia.Render.Signature (sigDecl)
import Tilia.Span

----------------------------------------------------------------------------
-- Settings

-- | What the printer needs to know about the module beyond its text.
--
-- Every field has a defensible empty value, and 'defaultSettings' uses
-- them, which is what keeps the printer usable with nothing configured. The
-- cost of the empty values is precision rather than correctness: without a
-- scope no operator chain is regrouped, and without the extensions a
-- handful of spacing decisions are made conservatively.
data Settings = Settings
  { -- | Extensions in force, from the module's own pragmas and from the
    -- package it belongs to.
    setExtensions :: Set Extension,
    -- | Whether this is a module or a Backpack signature.
    setSourceType :: SourceType,
    -- | What the module can see, if it could be worked out.
    setScope :: Maybe Scope
  }

-- | Settings that assert nothing.
defaultSettings :: Settings
defaultSettings =
  Settings
    { setExtensions = Set.empty,
      setSourceType = ModuleSource,
      setScope = Nothing
    }

----------------------------------------------------------------------------
-- Rendering

-- | Render a parsed module, comments and all.
renderModule :: Settings -> ParsedModule -> Doc
renderModule settings parsed =
  prologue (pmPrologue parsed)
    <> stackHeader
    <> attachComments loose (hsModule ctx pragmas (sorted hsMod))
  where
    hsMod = pmModule parsed
    (haddocks, loose') = splitHaddocks hsMod (pmComments parsed)
    plain = heldOff haddocks loose'
    (stackHeader, rest) = takeStackHeader (pmHeaderEnd parsed) plain
    (pragmas, loose) = takeHeaderPragmas (pmHeaderEnd parsed) rest

    sorted m =
      m
        { hsmodImports =
            normalizeImports
              (Set.member ImplicitPrelude (setExtensions settings))
              (hsmodImports m)
        }
    ctx =
      Ctx
        { ctxExtensions = setExtensions settings,
          ctxSourceType = setSourceType settings,
          ctxScope = setScope settings,
          ctxLineComments = indexOn (filter (not . closesItself) loose),
          ctxHaddocks = indexOn haddocks,
          ctxKnot = knot
        }

-- | Keep a comment from running into a Haddock.
--
-- A Haddock is printed from the syntax tree and a comment is placed against
-- whatever node it belongs to, so the two can come out on consecutive lines
-- however far apart they were written. Read together they look like one
-- block of prose, and they are not: one documents a declaration and the
-- other is a remark. An empty line is what says so.
heldOff :: [Comment] -> [Comment] -> [Comment]
heldOff haddocks = map holdOff
  where
    ends = Set.fromList (map (spanEndLine . commentSpan) haddocks)
    starts = Set.fromList (map (spanStartLine . commentSpan) haddocks)
    holdOff c =
      c
        { commentAfterGap =
            commentAfterGap c || Set.member (spanStartLine s - 1) ends,
          commentBeforeGap =
            commentBeforeGap c || Set.member (spanEndLine s + 1) starts
        }
      where
        s = commentSpan c

-- | The lines above the module, put back exactly as they were written.
--
-- They stand outside everything: no comment attaches to them, and no layout
-- decision may reach them. A @#!@ line that were indented, wrapped or moved
-- would stop being one.
prologue :: [Text] -> Doc
prologue = foldMap (\l -> txt l <> hardBreak)

-- | The knot: the printers that a module below their definition needs.
knot :: Knot
knot =
  Knot
    { knotExpr = hsExprIn,
      knotCmd = hsCmd,
      knotSplice = untypedSplice,
      knotSig = sigDecl,
      knotDecls = decls,
      knotDeclsGrouped = declsKeepingGroups
    }

-- | Separate the comments the syntax tree also knows about from the rest.
--
-- A doc comment the parser did not manage to attach to anything is not in
-- the tree, so nothing will print it, and it stays in the stream to be
-- attached by position like any other comment. Matching on positions rather
-- than on how the comment was written is what keeps those from being
-- dropped—and keeps the ones that /are/ in the tree from being printed
-- twice.
splitHaddocks ::
  HsModule GhcPs ->
  -- | Every comment in the module
  [Comment] ->
  -- | The ones the tree carries, and the ones it does not.
  --
  -- The first are not placed by the comment machinery at all: the printer
  -- reaches them through the node that owns them and keeps them here only so
  -- that it can reuse the text the author wrote rather than rebuilding it
  -- from the doc string. The second are the comment stream proper—attached
  -- by position, and the header pragmas taken out of them first.
  --
  -- This is also where it is settled what a doc comment's trigger is for. A
  -- Haddock the tree carries is going to be printed as one, so its trigger
  -- is tidied; one the tree does not carry is going to be printed as an
  -- ordinary comment, so its trigger is escaped and stops being a trigger.
  ([Comment], [Comment])
splitHaddocks hsMod = foldr sort' ([], [])
  where
    inTree = Set.fromList (map startPoint (haddockSpans hsMod))
    sort' c (docs, rest)
      | startPoint (commentSpan c) `Set.member` inTree,
        not (documentsNothing c) =
          (widenTrigger c : docs, rest)
      | otherwise = (docs, escapeTrigger c : rest)

-- | Index comments by where they begin.
indexOn :: [Comment] -> Map (Int, Int) Comment
indexOn cs = Map.fromList [(startPoint (commentSpan c), c) | c <- cs]
