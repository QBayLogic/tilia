{-# LANGUAGE OverloadedStrings #-}

-- | The module as its author wrote it.
--
-- Almost everything the formatter decides about layout is a question about
-- the source: what is on the line above a comment, whether two constructs
-- had an empty line between them, whether a directive stands between a
-- comment and the pragma under it. All these questions are asked in a
-- single place here.
module Tilia.Source
  ( -- * The source
    SourceType (..),
    Written (..),
    Source,
    sourceOf,

    -- * Its lines
    lineAt,
    blankAt,
    directiveAt,

    -- * Its comments
    comments,
  )
where

import Data.Char (isAsciiLower, isSpace)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs (HsModule)
import GHC.Hs.Extension (GhcPs)
import GHC.Parser.Annotation (LEpaComment)
import Tilia.Comments (Comment, commentsOf)

-- | Whether a file is a module or a Backpack signature.
data SourceType
  = ModuleSource
  | SignatureSource
  deriving (Eq, Show)

-- | The text of a module as its author wrote it.
--
-- Distinguished from the text handed to the parser because the two are the
-- same only when the preprocessor is not involved.
newtype Written = Written Text
  deriving (Eq, Show)

-- | A module's source in a form that facilitates querying.
data Source = Source
  { -- | The lines, numbered from one as the compiler numbers them.
    srcLines :: !(IntMap Text),
    -- | Every comment in the module, in source order.
    srcComments :: [Comment]
  }

-- | Read a module's source.
sourceOf ::
  -- | The input as written
  Written ->
  -- | Comments the syntax tree does not carry. See 'commentsOf'.
  [LEpaComment] ->
  -- | The result of parsing
  HsModule GhcPs ->
  Source
sourceOf (Written text) loose hsModule =
  Source
    { srcLines = IntMap.fromList (zip [1 ..] ls),
      srcComments = commentsOf ls loose hsModule
    }
  where
    ls = T.lines text

-- | The text of a line, if the module has one.
lineAt :: Int -> Source -> Maybe Text
lineAt n = IntMap.lookup n . srcLines

-- | Was this line empty?
--
-- A line the module does not have is not empty: past the end of a module
-- there is nothing to leave a gap between, and past the start there is
-- nothing above.
blankAt :: Int -> Source -> Bool
blankAt n = maybe False (T.all isSpace) . lineAt n

-- | Does this line hold a preprocessor directive?
directiveAt :: Int -> Source -> Bool
directiveAt n = maybe False opensWithHash . lineAt n
  where
    opensWithHash l = case T.uncons (T.stripStart l) of
      Just ('#', rest) -> maybe False (isAsciiLower . fst) (T.uncons (T.stripStart rest))
      _ -> False

-- | Every comment in the module, in source order.
comments :: Source -> [Comment]
comments = srcComments
