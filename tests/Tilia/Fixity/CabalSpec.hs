{-# LANGUAGE OverloadedStrings #-}

-- | Reading @exposed-modules@ out of a @.cabal@ file.
module Tilia.Fixity.CabalSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Tilia.Fixity.Cabal

spec :: Spec
spec = do
  describe "plain fields" $ do
    it "reads a one-line field" $
      exposed "library\n  exposed-modules: A.B, C.D\n"
        `shouldMatchList` ["A.B", "C.D"]

    it "reads a field spread over indented lines" $
      exposed "library\n  exposed-modules:\n    A.B\n    C.D\n    E\n"
        `shouldMatchList` ["A.B", "C.D", "E"]

    it "reads a mixture of commas and lines" $
      exposed "library\n  exposed-modules: A.B,\n    C.D\n"
        `shouldMatchList` ["A.B", "C.D"]

    it "is not confused by the field name's case" $
      exposed "library\n  Exposed-Modules: A.B\n" `shouldMatchList` ["A.B"]

    it "finds nothing when there is no such field" $
      exposed "library\n  build-depends: base\n" `shouldBe` []

  describe "what must not be picked up" $ do
    it "ignores other-modules" $
      exposed "library\n  exposed-modules: A\n  other-modules: B\n"
        `shouldMatchList` ["A"]

    it "ignores reexported-modules" $
      exposed "library\n  exposed-modules: A\n  reexported-modules: B\n"
        `shouldMatchList` ["A"]

    it "stops at the next field" $
      exposed "library\n  exposed-modules:\n    A\n  build-depends: base\n"
        `shouldMatchList` ["A"]

    it "ignores anything that is not a module name" $
      exposed "library\n  exposed-modules: A, base >=4, -Wall\n"
        `shouldMatchList` ["A"]

  describe "conditionals" $ do
    it "takes a branch nested inside an if" $
      exposed
        "library\n\
        \  exposed-modules: A\n\
        \  if flag(fancy)\n\
        \    exposed-modules: B\n"
        `shouldMatchList` ["A", "B"]

    it "takes both branches of an if/else" $
      exposed
        "library\n\
        \  if os(windows)\n\
        \    exposed-modules: W\n\
        \  else\n\
        \    exposed-modules: U\n"
        `shouldMatchList` ["W", "U"]

    it "takes a branch nested two deep" $
      exposed
        "library\n\
        \  if flag(a)\n\
        \    if flag(b)\n\
        \      exposed-modules: Deep\n"
        `shouldMatchList` ["Deep"]

    it "takes every library stanza, including named ones" $
      exposed
        "library\n\
        \  exposed-modules: Main.Lib\n\
        \\n\
        \library internal\n\
        \  exposed-modules: Internal.Lib\n"
        `shouldMatchList` ["Main.Lib", "Internal.Lib"]

  describe "comments" $ do
    it "does not let one at the margin cut a module list short" $
      exposed "library\n  exposed-modules:\n    A\n--    B\n    C\n"
        `shouldMatchList` ["A", "C"]

    it "does not count a module somebody commented out" $
      exposed "library\n  exposed-modules:\n    A\n    -- B\n    C\n"
        `shouldMatchList` ["A", "C"]

    it "keeps reading source directories past one" $
      sourceDirs "library\n  hs-source-dirs: src\n-- a comment\ntest-suite t\n  hs-source-dirs: tests\n"
        `shouldBe` ["src", "tests", "."]

  describe "where a component with no hs-source-dirs lives" $ do
    it "offers the package directory even when other components name one" $
      sourceDirs "library\n  build-depends: base\ntest-suite t\n  hs-source-dirs: tests\n"
        `shouldBe` ["tests", "."]

    it "offers it last, so a named directory is tried first" $
      last (sourceDirs "library\n  hs-source-dirs: src\n") `shouldBe` "."

    it "offers it once when it is named as well" $
      sourceDirs "library\n  hs-source-dirs: .\n" `shouldBe` ["."]

    it "offers it when nothing names anything" $
      sourceDirs "library\n  build-depends: base\n" `shouldBe` ["."]

  describe "the union is deliberate" $
    it "does not need to know which branch a build would take" $ do
      -- Both are reported. A module the real build does not expose cannot
      -- be imported by the project being formatted, so nothing will ever
      -- ask about it, and the extra entry is dead weight rather than a
      -- wrong answer.
      let both =
            exposed
              "library\n\
              \  if impl(ghc >= 9.6)\n\
              \    exposed-modules: New\n\
              \  else\n\
              \    exposed-modules: Old\n"
      both `shouldMatchList` ["New", "Old"]

exposed :: Text -> [Text]
exposed = exposedModules . T.pack . T.unpack
