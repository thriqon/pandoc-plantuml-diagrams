{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module    : Text.Pandoc.PlantUML.Filter
--
-- pandoc-plantuml-diagrams is a filter for Pandoc that automatically
-- renders UML diagrams with PlantUML.
module Text.Pandoc.PlantUML.Filter (processBlocks) where

import Text.Pandoc.JSON
import Control.Monad

import Text.Pandoc.PlantUML.Filter.Types
import Text.Pandoc.PlantUML.Filter.FileNameGenerator
import Text.Pandoc.PlantUML.Filter.Formats
import Text.Pandoc.PlantUML.Filter.OutputBlock

-- | Processes a block in the context of the give format.
-- The call syntax is compatible with the json filter provided
-- by Pandoc.
--
processBlocks :: ImageIO m => Maybe Format -> Block -> m Block
processBlocks (Just format) block@(CodeBlock attr@(_, classes, _) contents)
  | "uml" `elem` classes       = do
    ensureRendered imageFileName (DiagramSource contents)
    return $ resultBlock imageFileName attr
  | otherwise                  = return block
  where imageFileName = ImageFileName (fileNameForSource (DiagramSource contents)) (imageFormatTypeFor format)
processBlocks _ x              = return x

ensureRendered :: ImageIO m => ImageFileName -> DiagramSource -> m ()
ensureRendered imageFileName source = inCaseNotExists imageFileName $ renderImage imageFileName source

inCaseNotExists :: ImageIO m => ImageFileName -> m () -> m ()
inCaseNotExists fileName action = doesImageExist fileName >>= flip unless action
