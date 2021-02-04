{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.PlantUML.FilterSpec where

import Test.Hspec
import Text.Pandoc.Definition

import Text.Pandoc.PlantUML.Filter.Types
import Text.Pandoc.PlantUML.Filter.FileNameGenerator
import Text.Pandoc.PlantUML.Filter
import Control.Monad.State.Lazy as S
import Data.Text

type FakeImageIO = S.State WorldInfo

data WorldInfo = WorldInfo {
    renderedImages :: [(String, DiagramSource)]
  , existingImages :: [String]
}

baseNameForDiagramAsd = fileNameForSource (DiagramSource "asd")
baseNameForDiagramDsa = fileNameForSource (DiagramSource "dsa")

def = WorldInfo [] []

instance ImageIO (S.State WorldInfo) where
  doesImageExist fileName     = do
    st <- S.get
    return $ (show fileName) `elem` (existingImages st)
  renderImage fileName source = do
    st <- S.get
    let oldImages = renderedImages st
    S.put st { renderedImages = ((show fileName), source) : oldImages}


runProcessingIn :: WorldInfo -> Text -> Block -> (Block, WorldInfo)
runProcessingIn wi format block = S.runState (processBlocks (Just (Format format)) block) wi


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Text.Pandoc.PlantUML.Filter" $ do
    it "renders a diagram with some content" $ do
      let (_, endState) = runProcessingIn def "manpage" (CodeBlock ("id", ["uml"], []) "dsa")
      (renderedImages endState) `shouldBe` [(baseNameForDiagramDsa ++ ".png", (DiagramSource "dsa"))]
    it "skips other blocks" $ do
      let (_, endState) = runProcessingIn def "manpage" $ Para [Str "@startuml asd @enduml"]
      (renderedImages endState) `shouldBe` []
    it "doesn't re-render an already rendered diagram" $ do
      let world = def { existingImages = [baseNameForDiagramAsd ++ ".eps"]}
      let (_, endState) = runProcessingIn world "latex" $ (CodeBlock ("id", ["uml"], []) "asd")
      (renderedImages endState) `shouldBe` []
    it "rerenders a diagram for a different format" $ do
      let world = def { existingImages = [baseNameForDiagramAsd ++ ".eps"]}
      let (_, endState) = runProcessingIn world "html" $ (CodeBlock ("id", ["uml"], []) "asd")
      (renderedImages endState) `shouldBe` [(baseNameForDiagramAsd ++ ".png", (DiagramSource "asd"))]
