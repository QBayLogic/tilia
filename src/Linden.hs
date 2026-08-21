-- | A formatter for Haskell source code. This module exposes the official
-- stable API; other modules may not be as reliable.
module Linden
  ( linden,
  )
where

import Data.Text (Text)

-- | Format a 'Text' value containing a Haskell module.
linden :: Text -> Text
linden = id
