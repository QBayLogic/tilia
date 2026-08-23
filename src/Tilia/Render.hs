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
import GHC.Hs (HsModule (..))
import GHC.Hs.Extension (GhcPs)
import GHC.LanguageExtensions.Type (Extension)
import Tilia.Imports (normalizeImports)
import Tilia.Comments (Comment (..), CommentStyle (..))
import Tilia.Comments.Attach (attachComments)
import Tilia.Fixity (Scope)
import Tilia.Parser (ParsedModule (..))
import Tilia.Printer.Combinators
import Tilia.Render.Context
import Tilia.Render.Declaration (decls, declsKeepingGroups)
import Tilia.Render.Expression (hsCmd, hsExprIn, untypedSplice)
import Tilia.Render.Haddock (haddockSpans)
import Tilia.Render.Header (hsModule, takeHeaderPragmas)
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
  attachComments loose (hsModule ctx pragmas (sorted hsMod))
  where
    hsMod = pmModule parsed
    sorted m = m {hsmodImports = normalizeImports loose (hsmodImports m)}
    (haddocks, plain) = splitHaddocks hsMod (pmComments parsed)
    (pragmas, loose) = takeHeaderPragmas (pmHeaderEnd parsed) plain
    ctx =
      Ctx
        { ctxExtensions = setExtensions settings,
          ctxSourceType = setSourceType settings,
          ctxScope = setScope settings,
          ctxLineComments = indexOn commentSpan (filter takesWholeLine loose),
          ctxHaddocks = indexOn id haddocks,
          ctxKnot = knot
        }

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
  ([Comment], [Comment])
splitHaddocks hsMod = foldr sort' ([], [])
  where
    inTree = Set.fromList (map startOfSpan (haddockSpans hsMod))
    sort' c (docs, rest)
      | startOfSpan (commentSpan c) `Set.member` inTree = (c : docs, rest)
      | otherwise = (docs, c : rest)

-- | Does this comment own the rest of the line it lands on?
--
-- A single-line block comment does not: @f {- here -} x@ is fine as it
-- stands. Everything else does, and a construct holding one cannot be put on
-- one line.
takesWholeLine :: Comment -> Bool
takesWholeLine c = case commentStyle c of
  BlockComment -> spanStartLine s /= spanEndLine s
  _ -> True
  where
    s = commentSpan c

-- | Index comments by where they begin, keeping whatever of each is wanted.
indexOn :: (Comment -> a) -> [Comment] -> Map (Int, Int) a
indexOn f cs = Map.fromList [(startOfSpan (commentSpan c), f c) | c <- cs]

startOfSpan :: Span -> (Int, Int)
startOfSpan s = (spanStartLine s, spanStartColumn s)
