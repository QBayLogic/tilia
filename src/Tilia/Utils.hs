{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Miscellaneous utilities.
module Tilia.Utils
  ( quietly,
    attempted,
  )
where

import Control.Exception (SomeException, displayException, try)
import Data.Text (Text)
import Data.Text qualified as T

-- | Run an action, falling back on the given value if it throws.
quietly :: a -> IO a -> IO a
quietly fallback action =
  try action >>= \case
    Left (_ :: SomeException) -> pure fallback
    Right a -> pure a

-- | Run an action, keeping what it threw rather than a fallback.
attempted :: IO a -> IO (Either Text a)
attempted action =
  try action >>= \case
    Left (e :: SomeException) -> pure (Left (T.pack (displayException e)))
    Right a -> pure (Right a)
