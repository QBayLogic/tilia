-- | Constructs that can stand as the body of an enclosing one.
module Tilia.Doc.Body
  ( Body (..),
    attachBody,
  )
where

import Tilia.Doc.Combinators

-- | Something that can appear as the body of an enclosing construct.
class Body a where
  -- | Print it.
  printBody :: a -> Doc

  -- | Whether it absorbs its own line break.
  bodyPlacement :: a -> Placement

-- | Print a body and join it to whatever precedes it.
--
-- This is the whole of what an enclosing construct needs, which is why it
-- is worth having: a caller that reaches for 'printBody' and
-- 'bodyPlacement' separately is about to reimplement it.
attachBody :: (Body a) => a -> Doc
attachBody x = attach (bodyPlacement x) (printBody x)
