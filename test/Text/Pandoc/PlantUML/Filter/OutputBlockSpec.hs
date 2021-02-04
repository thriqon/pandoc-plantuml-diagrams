{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.PlantUML.Filter.OutputBlockSpec where

import Test.Hspec
import Text.Pandoc.PlantUML.Filter.OutputBlock
import Text.Pandoc.PlantUML.Filter.Types
import Text.Pandoc.Definition

-- resultBlock :: ImageFileName -> Attr -> Block

spec :: Spec
spec = do
  describe "Text.Pandoc.PlantUML.Filter.OutputBlock" $ do
    it "renders into a Paragraph containing an Image" $ do
      let b = resultBlock (ImageFileName "asd" "png") ("id", [], [])
      case b of
        Para [(Image nullAttr _ (src, _)), _ ]   -> do
          src `shouldBe` "asd.png"
        otherwise                -> expectationFailure "should be an image in a paragraph"
    it "renders into a Paragraph containing an ID tag" $ do
      let b = resultBlock (ImageFileName "asd" "png") ("fig:id", [], [])
      case b of
        Para [_, (Str id) ]       -> do
          id `shouldBe` "{#fig:id}"
        otherwise                -> expectationFailure "should be an image in a paragraph"


main :: IO()
main = hspec spec
