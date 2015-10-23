
-----------------------------------------------------------------------
-- |
-- Module    : Text.Pandoc.PlantUML.Filter
--
-- pandoc-plantuml-diagrams is a filter for Pandoc that automatically
-- renders UML diagrams with PlantUML.
-----------------------------------------------------------------------
module Text.Pandoc.PlantUML.Filter (processBlocks) where

import Text.Pandoc.JSON
import Control.Monad

import Text.Pandoc.PlantUML.Filter.Types
import Text.Pandoc.PlantUML.Filter.FileNameGenerator

processBlocks :: ImageIO m => Maybe Format -> Block -> m Block
processBlocks (Just format) (CodeBlock (id, ["uml"], keyValues) contents) = do
  ensureRendered imageFileName contents
  return $ Para [(Image altTag ((show imageFileName), "fig:")), (Str ("{#" ++ id ++ "}"))]
    where
      imageFileName = ImageFileName (fileNameForSource contents) (imageFormatTypeFor format)
      altTag = case findAltText keyValues of
                 Just f -> [Str f]
                 Nothing -> []
processBlocks _ x = return x

-- | The image file type to be used for the given output format.
-- EPS is used for latex outputs, as it provides lossless scalability
-- All other output formats use PNG for now.
imageFormatTypeFor :: Format -> ImageFormat
imageFormatTypeFor (Format "latex") =  "eps"
imageFormatTypeFor _                =  "png"

-- | Finds the caption in an image, if present.
findAltText :: [(String, String)] -> Maybe String
findAltText = lookup "caption"

altTag :: Attr -> [Inline]
altTag (_, _, map) = case findAltText map of
  Just f   -> [Str f]
  Nothing  -> []

ensureRendered :: ImageIO m => ImageFileName -> DiagramSource -> m ()
ensureRendered imageFileName source = do
  exists <- doesImageExist imageFileName
  when (not exists) $ renderImage imageFileName source
