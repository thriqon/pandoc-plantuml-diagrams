{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.PlantUML.Filter.TypesSpec where

import Test.Hspec
import Text.Pandoc.PlantUML.Filter.Types

spec :: Spec
spec = do
  describe "Text.Pandoc.PlantUML.Filter.Types" $ do
    it "can show an image filename correctly" $ do
      (show (ImageFileName "base" "eps")) `shouldBe` "base.eps"


main :: IO ()
main = hspec spec
