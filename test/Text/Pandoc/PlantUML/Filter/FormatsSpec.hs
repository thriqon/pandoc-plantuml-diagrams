

module Text.Pandoc.PlantUML.Filter.FormatsSpec where

import Test.Hspec
import Text.Pandoc.Definition
import Text.Pandoc.PlantUML.Filter.Formats

formatFor :: String -> String
formatFor = imageFormatTypeFor . Format

shouldBeFormattedAs :: String -> String -> Expectation
shouldBeFormattedAs docFormat imageFormat = (imageFormatTypeFor (Format docFormat)) `shouldBe` imageFormat

spec :: Spec
spec = do
  describe "Text.Pandoc.PlantUML.Filter.Formats" $ do
    it "gives EPS for latex" $ do
      "latex" `shouldBeFormattedAs` "eps"
    it "gives PNG for html5" $ do
      "html5" `shouldBeFormattedAs` "png"


main :: IO ()
main = hspec spec
