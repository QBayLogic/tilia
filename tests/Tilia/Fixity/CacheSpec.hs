{-# LANGUAGE OverloadedStrings #-}

-- | The on-disk cache of what was read out of a package.
module Tilia.Fixity.CacheSpec (spec) where

import Data.Map.Strict qualified as Map
import System.Environment (setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Tilia.Fixity
import Tilia.Fixity.Cache

spec :: Spec
spec = around withIsolatedCache $ do
  describe "modules" $ do
    it "remembers a package's module list" $ \cache -> do
      storeModules cache "thing-1.0-abc" ["A.B", "C"]
      cachedModules cache "thing-1.0-abc" `shouldReturn` Just ["A.B", "C"]

    it "knows nothing about a package it was never told about" $ \cache ->
      cachedModules cache "absent-1.0" `shouldReturn` Nothing

    it "remembers an empty list as a fact, not as absence" $ \cache -> do
      storeModules cache "empty-1.0" []
      cachedModules cache "empty-1.0" `shouldReturn` Just []

  describe "fixities" $ do
    it "round-trips every direction" $ \cache -> do
      let fixities =
            Map.fromList
              [ (OpName "<+>", Fixity LeftAssoc 6),
                (OpName ">>=", Fixity RightAssoc 1),
                (OpName "===", Fixity NoAssoc 4)
              ]
      storeFixities cache "thing-1.0" "A.B" fixities
      cachedFixities cache "thing-1.0" "A.B" `shouldReturn` Just fixities

    it "round-trips the extremes of precedence" $ \cache -> do
      let fixities =
            Map.fromList
              [ (OpName "!", Fixity LeftAssoc 0),
                (OpName "?", Fixity LeftAssoc 9)
              ]
      storeFixities cache "thing-1.0" "Edges" fixities
      cachedFixities cache "thing-1.0" "Edges" `shouldReturn` Just fixities

    it "remembers that a module declares nothing" $ \cache -> do
      -- The distinction this cache has to preserve: an empty map is an
      -- answer, and re-deriving it costs as much as any other.
      storeFixities cache "thing-1.0" "Quiet" Map.empty
      cachedFixities cache "thing-1.0" "Quiet" `shouldReturn` Just Map.empty

    it "knows nothing about a module it was never told about" $ \cache ->
      cachedFixities cache "thing-1.0" "Absent" `shouldReturn` Nothing

    it "keeps packages apart" $ \cache -> do
      storeFixities cache "a-1.0" "M" (Map.fromList [(OpName "<>", Fixity RightAssoc 6)])
      storeFixities cache "b-1.0" "M" Map.empty
      a <- cachedFixities cache "a-1.0" "M"
      b <- cachedFixities cache "b-1.0" "M"
      (Map.size <$> a, Map.size <$> b) `shouldBe` (Just 1, Just 0)

    it "treats a different hash in the key as a different package" $ \cache -> do
      -- This is what binds cached answers to the bytes they came from: a
      -- changed tarball is filed under a different key and simply misses.
      storeFixities cache "thing-1.0-aaaa" "M" (Map.fromList [(OpName "!", Fixity LeftAssoc 9)])
      cachedFixities cache "thing-1.0-bbbb" "M" `shouldReturn` Nothing

    it "overwrites a previous answer for the same key" $ \cache -> do
      storeFixities cache "thing-1.0" "M" (Map.fromList [(OpName "!", Fixity LeftAssoc 9)])
      storeFixities cache "thing-1.0" "M" (Map.fromList [(OpName "!", Fixity RightAssoc 3)])
      cachedFixities cache "thing-1.0" "M"
        `shouldReturn` Just (Map.fromList [(OpName "!", Fixity RightAssoc 3)])

  describe "module names with dots" $
    it "files a deeply qualified module without confusion" $ \cache -> do
      storeFixities cache "thing-1.0" "A.B.C.D" (Map.fromList [(OpName "%", Fixity NoAssoc 5)])
      cachedFixities cache "thing-1.0" "A.B.C.D"
        `shouldReturn` Just (Map.fromList [(OpName "%", Fixity NoAssoc 5)])

-- | Give each test its own cache directory, so nothing leaks between them
-- or into the developer's real cache.
withIsolatedCache :: (Cache -> IO ()) -> IO ()
withIsolatedCache action =
  withSystemTempDirectory "tilia-cache" $ \dir -> do
    setEnv "XDG_CACHE_HOME" dir
    opened <- openCache
    unsetEnv "XDG_CACHE_HOME"
    case opened of
      Nothing -> expectationFailure "could not open a cache in a temporary directory"
      Just cache -> action cache
