{-# LANGUAGE OverloadedStrings #-}

-- | Reading what the compiler says it has.
module Tilia.Fixity.PackageDbSpec (spec) where

import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Test.Hspec
import Tilia.Fixity.PackageDb

spec :: Spec
spec = do
  describe "one record of ghc-pkg dump" $ do
    it "reads the directories a package's interfaces are in" $
      dirsOf [("import-dirs", "/opt/ghc/lib/base-4.20.2.0")]
        `shouldBe` Just ["/opt/ghc/lib/base-4.20.2.0"]

    it "puts the package root where the registration left a variable" $
      dirsOf
        [ ("pkgroot", "\"/opt/ghc/lib\""),
          ("import-dirs", "${pkgroot}/../lib/base-4.20.2.0")
        ]
        `shouldBe` Just ["/opt/ghc/lib/../lib/base-4.20.2.0"]

    it "leaves the variable alone when the record does not say" $
      dirsOf [("import-dirs", "${pkgroot}/../lib/base-4.20.2.0")]
        `shouldBe` Just ["${pkgroot}/../lib/base-4.20.2.0"]

    it "roots every directory a package names" $
      dirsOf
        [ ("pkgroot", "/opt/ghc/lib"),
          ("import-dirs", "${pkgroot}/one ${pkgroot}/two")
        ]
        `shouldBe` Just ["/opt/ghc/lib/one", "/opt/ghc/lib/two"]

    it "says nothing of a record that names no package" $
      fromFields (Map.fromList [("import-dirs", "/opt/ghc/lib")])
        `shouldBe` Nothing

  describe "what this compiler reports" $
    it "leaves no path variable in any import directory" $ do
      installed <- readInstalledPackages
      [ dir
        | p <- installedPackages installed,
          dir <- ipImportDirs p,
          "${" `isInfixOf` dir
        ]
        `shouldBe` []

-- | The import directories one record amounts to, given its fields.
dirsOf :: [(Text, Text)] -> Maybe [FilePath]
dirsOf fields =
  ipImportDirs
    <$> fromFields (Map.fromList (named <> fields))
  where
    named = [("name", "base"), ("version", "4.20.2.0")]
