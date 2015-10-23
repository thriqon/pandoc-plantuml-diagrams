{-# LANGUAGE FlexibleInstances #-}

module Text.Pandoc.PlantUML.FilterSpec where

import Test.Hspec

import Text.Pandoc.PlantUML.Filter.Types
import Control.Monad.State.Lazy as S


type FakeImageIO = S.State FakeState

data FakeState = FS {
    renderedImages :: [(String, DiagramSource)]
  , existingImages :: [String]
}

oneExisting :: FakeState
oneExisting = FS {
  renderedImages = [],
  existingImages = [".rendered.601457f65ec041ec8f4087ffefa6bc191a129ff9.eps"]
}

instance ImageIO (S.State FakeState) where
  doesImageExist fileName     = do
    st <- S.get
    return $ (show fileName) `elem` (existingImages st)
  renderImage fileName source = do
    st <- S.get
    let oldImages = renderedImages st
    S.put st { renderedImages = ((show fileName), source) : oldImages}


runFakeImageIO :: b -> State b a -> (a, b)
runFakeImageIO = flip S.runState



main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "the testsuite" $ do
    it "runs" $ do
      5 `shouldBe` 5 
