{-# LANGUAGE OverloadedStrings #-}

-- | The module as its author wrote it.
--
-- Almost everything the formatter decides about layout is a question about
-- the source: what is on the line above a comment, whether two constructs
-- had an empty line between them, whether a comment was written inside this
-- import. All these questions are asked in a single place here.
module Tilia.Source
  ( -- * The source
    Written (..),
    Source,
    sourceOf,

    -- * Its lines
    lineAt,
    blankAt,

    -- * Its comments
    comments,
  )
where

import Data.Char (isSpace)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs (HsModule)
import GHC.Hs.Extension (GhcPs)
import Tilia.Comments (Comment, commentsOf)

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
sourceOf :: Written -> HsModule GhcPs -> Source
sourceOf (Written text) hsModule =
  Source
    { srcLines = IntMap.fromList (zip [1 ..] (T.lines text)),
      srcComments = commentsOf text hsModule
    }

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

-- | Every comment in the module, in source order.
comments :: Source -> [Comment]
comments = srcComments
