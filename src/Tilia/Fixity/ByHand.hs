{-# LANGUAGE OverloadedStrings #-}

-- | Fixities for the few modules whose source defeats us.
module Tilia.Fixity.ByHand
  ( byHandFixities,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Tilia.Fixity

-- | The modules, and the operators they declare.
byHandFixities :: Map Text (Map OpName Fixity)
byHandFixities =
  Map.fromList
    [ -- @Test.QuickCheck.Property@ invokes a macro it defines:
      -- @WITNESSES(:: [Witness])@ stands in the middle of a record, and
      -- only @cpp@ can expand it. No configuration of the module is
      -- Haskell, so there is nothing to parse in any of them.
      entry
        "Test.QuickCheck.Property"
        [ ("==>", RightAssoc, 0),
          (".&.", RightAssoc, 1),
          (".&&.", RightAssoc, 1),
          (".||.", RightAssoc, 1),
          ("===", NoAssoc, 4),
          ("=/=", NoAssoc, 4)
        ]
    ]
  where
    entry name ops =
      (name, Map.fromList [(OpName o, Fixity d p) | (o, d, p) <- ops])
