{-# LANGUAGE OverloadedStrings #-}

module LindenSpec (spec) where

import Data.Text (Text)
import Linden (linden)
import Test.Hspec

spec :: Spec
spec =
  describe "linden" $
    it "is idempotent" $
      linden (linden sample) `shouldBe` linden sample
  where
    sample :: Text
    sample = "module Main where\n"
