{-# LANGUAGE OverloadedStrings #-}

-- | Reading a @.cabal@ file's fields without a cabal parser.
module Tilia.Fixity.CabalSpec (spec) where

import Data.Text (Text)
import GHC.LanguageExtensions.Type (Extension (..))
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
    it "takes other-modules too, which a re-export may lead into" $
      exposed "library\n  exposed-modules: A\n  other-modules: B\n"
        `shouldMatchList` ["A", "B"]

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

  describe "what the package puts in force" $ do
    it "reads an extension the .cabal turns on" $
      extensions "library\n  default-extensions: LambdaCase\n"
        `shouldSatisfy` elem LambdaCase

    it "reads several, however they are written" $ do
      let found = extensions "library\n  default-extensions:\n    LambdaCase\n    MultiWayIf, BlockArguments\n"
      found `shouldSatisfy` elem LambdaCase
      found `shouldSatisfy` elem MultiWayIf
      found `shouldSatisfy` elem BlockArguments

    it "takes one back that the .cabal turns off" $
      extensions "library\n  default-extensions: ImplicitPrelude, NoImplicitPrelude\n"
        `shouldSatisfy` notElem ImplicitPrelude

    it "starts from what the language edition puts in force" $ do
      extensions "library\n  default-language: GHC2021\n"
        `shouldSatisfy` elem TypeOperators
      extensions "library\n  default-language: Haskell2010\n"
        `shouldSatisfy` notElem TypeOperators

    it "takes what every edition in the file puts in force" $
      extensions
        "library\n\
        \  default-language: Haskell2010\n\
        \test-suite spec\n\
        \  default-language: GHC2021\n"
        `shouldSatisfy` elem TypeOperators

    it "still has the earlier edition's own extensions" $
      extensions
        "library\n\
        \  default-language: Haskell2010\n\
        \test-suite spec\n\
        \  default-language: GHC2021\n"
        `shouldSatisfy` elem ImplicitPrelude

    it "passes over a name no compiler knows" $
      extensions "library\n  default-extensions: LambdaCase, NotAnExtension\n"
        `shouldSatisfy` elem LambdaCase

    it "takes every component's, since it does not know which one asks" $ do
      let found =
            extensions
              "library\n  default-extensions: LambdaCase\ntest-suite t\n  default-extensions: MultiWayIf\n"
      found `shouldSatisfy` elem LambdaCase
      found `shouldSatisfy` elem MultiWayIf

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

-- | The modules a @.cabal@ of this shape holds.
exposed :: Text -> [Text]
exposed = containedModules

-- | What a @.cabal@ of this shape puts in force.
extensions :: Text -> [Extension]
extensions = declaredExtensions
