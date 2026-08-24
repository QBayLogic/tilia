{-# LANGUAGE OverloadedStrings #-}

-- | Reading a module, and what can be told about one before reading it.
module Tilia.ParserSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Tilia.Parser

spec :: Spec
spec =
  describe "movesPositions" $ do
    describe "says so" $ do
      for_'
        [ ("a LINE pragma", "{-# LINE 1 \"Other.hs\" #-}\n"),
          ("a COLUMN pragma", "{-# COLUMN 20 #-}\n"),
          ("one written in lower case", "{-# line 1 \"Other.hs\" #-}\n"),
          ("one written without spaces", "{-#LINE 1 \"Other.hs\"#-}\n"),
          ("one written over several lines", "{-# LINE 1\n      \"Other.hs\" #-}\n"),
          ("one below the header", "module M where\nx = 1\n{-# LINE 9 \"O.hs\" #-}\n"),
          ("one among pragmas that do not move anything", langThenLine)
        ]
        (\source -> movesPositions source `shouldBe` True)

    describe "says nothing of" $ do
      for_'
        [ ("a module with no pragma at all", "module M where\nx = 1\n"),
          ("a LANGUAGE pragma", "{-# LANGUAGE LambdaCase #-}\nmodule M where\n"),
          ("an INLINE pragma", "module M where\n{-# INLINE f #-}\nf = id\n"),
          ("a pragma whose name merely starts with one", "{-# LINEAR 1 #-}\n"),
          ("an unclosed pragma", "{-# LINE 1 \"Other.hs\"\n"),
          ("the word in a comment", "-- {-* LINE 1 *-}\nmodule M where\n")
        ]
        (\source -> movesPositions source `shouldBe` False)
  where
    for_' cases expect = mapM_ (\(what, source) -> it what (expect source)) cases

    langThenLine :: Text
    langThenLine = "{-# LANGUAGE LambdaCase #-}\nmodule M where\n{-# LINE 3 \"O.hs\" #-}\n"
