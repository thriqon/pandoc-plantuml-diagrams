

module Text.Pandoc.PlantUML.Filter.FileNameGeneratorSpec where

import Test.Hspec
import Text.Pandoc.PlantUML.Filter.FileNameGenerator

spec :: Spec
spec = do
  describe "Text.Pandoc.PlantUML.Filter.FileNameGenerator" $ do
    it "generates the correct name for an empty source" $ do
      (fileNameForSource "") `shouldBe` ".rendered.da39a3ee5e6b4b0d3255bfef95601890afd80709"
    it "generates the correct name for 'hello'" $ do
      (fileNameForSource "hello") `shouldBe` ".rendered.aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d"

main :: IO ()
main = hspec spec
