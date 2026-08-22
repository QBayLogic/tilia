{-# LANGUAGE OverloadedStrings #-}

module TiliaSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Tilia (tilia)

spec :: Spec
spec =
  describe "tilia" $
    it "is idempotent" $
      tilia (tilia sample) `shouldBe` tilia sample
  where
    sample :: Text
    sample = "module Main where\n"
