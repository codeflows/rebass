-- http://hackage.haskell.org/packages/archive/hspec/0.3.0/doc/html/Test-Hspec.html

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.HUnit
import Test.QuickCheck hiding (property)
import Test.HUnit

formatPhoneNumber :: String -> String
formatPhoneNumber number = undefined

mySpecs = describe "Formatting phone number" [
    it "removes spaces"
      (formatPhoneNumber "040 5394434" == "0405394434")
  ]

main = hspec mySpecs
