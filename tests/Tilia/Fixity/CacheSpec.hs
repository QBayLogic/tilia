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
spec = do
  tokens
  around withIsolatedCache $ do
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
        storeFixities cache "thing-1.0" "A.B" (Declares fixities)
        cachedFixities cache "thing-1.0" "A.B" `shouldReturn` Just (Declares fixities)

      it "round-trips the extremes of precedence" $ \cache -> do
        let fixities =
              Map.fromList
                [ (OpName "!", Fixity LeftAssoc 0),
                  (OpName "?", Fixity LeftAssoc 9)
                ]
        storeFixities cache "thing-1.0" "Edges" (Declares fixities)
        cachedFixities cache "thing-1.0" "Edges" `shouldReturn` Just (Declares fixities)

      it "remembers that a module declares nothing" $ \cache -> do
        storeFixities cache "thing-1.0" "Quiet" (Declares Map.empty)
        cachedFixities cache "thing-1.0" "Quiet" `shouldReturn` Just (Declares Map.empty)

      it "remembers that a module could not be read" $ \cache -> do
        storeFixities cache "thing-1.0" "Opaque" Unreadable
        cachedFixities cache "thing-1.0" "Opaque" `shouldReturn` Just Unreadable

      it "tells an unread module from one it was never told about" $ \cache -> do
        storeFixities cache "thing-1.0" "Opaque" Unreadable
        unread <- cachedFixities cache "thing-1.0" "Opaque"
        never <- cachedFixities cache "thing-1.0" "Absent"
        (unread, never) `shouldBe` (Just Unreadable, Nothing)

      it "tells an unread module from one that declares nothing" $ \cache -> do
        storeFixities cache "thing-1.0" "Opaque" Unreadable
        storeFixities cache "thing-1.0" "Quiet" (Declares Map.empty)
        opaque <- cachedFixities cache "thing-1.0" "Opaque"
        quiet <- cachedFixities cache "thing-1.0" "Quiet"
        (opaque, quiet) `shouldBe` (Just Unreadable, Just (Declares Map.empty))

      it "replaces an unread answer once the module can be read" $ \cache -> do
        storeFixities cache "thing-1.0" "M" Unreadable
        storeFixities cache "thing-1.0" "M" (Declares (Map.fromList [(OpName "!", Fixity LeftAssoc 9)]))
        cachedFixities cache "thing-1.0" "M"
          `shouldReturn` Just (Declares (Map.fromList [(OpName "!", Fixity LeftAssoc 9)]))

      it "knows nothing about a module it was never told about" $ \cache ->
        cachedFixities cache "thing-1.0" "Absent" `shouldReturn` Nothing

      it "keeps packages apart" $ \cache -> do
        let ops = Map.fromList [(OpName "<>", Fixity RightAssoc 6)]
        storeFixities cache "a-1.0" "M" (Declares ops)
        storeFixities cache "b-1.0" "M" (Declares Map.empty)
        a <- cachedFixities cache "a-1.0" "M"
        b <- cachedFixities cache "b-1.0" "M"
        (a, b) `shouldBe` (Just (Declares ops), Just (Declares Map.empty))

      it "treats a different hash in the key as a different package" $ \cache -> do
        storeFixities cache "thing-1.0-aaaa" "M" (Declares (Map.fromList [(OpName "!", Fixity LeftAssoc 9)]))
        cachedFixities cache "thing-1.0-bbbb" "M" `shouldReturn` Nothing

      it "overwrites a previous answer for the same key" $ \cache -> do
        storeFixities cache "thing-1.0" "M" (Declares (Map.fromList [(OpName "!", Fixity LeftAssoc 9)]))
        storeFixities cache "thing-1.0" "M" (Declares (Map.fromList [(OpName "!", Fixity RightAssoc 3)]))
        cachedFixities cache "thing-1.0" "M"
          `shouldReturn` Just (Declares (Map.fromList [(OpName "!", Fixity RightAssoc 3)]))

    describe "module names with dots" $
      it "files a deeply qualified module without confusion" $ \cache -> do
        storeFixities cache "thing-1.0" "A.B.C.D" (Declares (Map.fromList [(OpName "%", Fixity NoAssoc 5)]))
        cachedFixities cache "thing-1.0" "A.B.C.D"
          `shouldReturn` Just (Declares (Map.fromList [(OpName "%", Fixity NoAssoc 5)]))

-- | What an answer of \"could not be read\" is tied to, and what it is not.
tokens :: Spec
tokens = around withIsolatedDirectory $ do
  it "does not offer an unread answer written under another token" $ \dir -> do
    before' <- open dir (PlanToken "one")
    storeFixities before' "thing-1.0" "M" Unreadable
    after' <- open dir (PlanToken "two")
    cachedFixities after' "thing-1.0" "M" `shouldReturn` Nothing

  it "still offers one written under the same token" $ \dir -> do
    before' <- open dir (PlanToken "one")
    storeFixities before' "thing-1.0" "M" Unreadable
    again <- open dir (PlanToken "one")
    cachedFixities again "thing-1.0" "M" `shouldReturn` Just Unreadable

  it "keeps an answer that was read, whatever the token" $ \dir -> do
    let fixities = Map.fromList [(OpName "<+>", Fixity RightAssoc 6)]
    before' <- open dir (PlanToken "one")
    storeFixities before' "thing-1.0" "M" (Declares fixities)
    after' <- open dir (PlanToken "two")
    cachedFixities after' "thing-1.0" "M" `shouldReturn` Just (Declares fixities)

-- | Give each test its own cache directory, so nothing leaks between them
-- or into the developer's real cache.
withIsolatedCache :: (Cache -> IO ()) -> IO ()
withIsolatedCache action =
  withIsolatedDirectory (\dir -> open dir (PlanToken "plan") >>= action)

withIsolatedDirectory :: (FilePath -> IO ()) -> IO ()
withIsolatedDirectory = withSystemTempDirectory "tilia-cache"

-- | Open a cache in a given directory, under a given token.
open :: FilePath -> PlanToken -> IO Cache
open dir token = do
  setEnv "XDG_CACHE_HOME" dir
  opened <- openCache token
  unsetEnv "XDG_CACHE_HOME"
  case opened of
    Nothing -> fail "could not open a cache in a temporary directory"
    Just cache -> pure cache
