{-# LANGUAGE OverloadedStrings #-}
-- | Module : Text.Pandoc.PlantUML.Filter.Formats
-- Determines the image type to be used for one particular
-- pandoc output format.
--
-- Currently uses EPS for latex-based outputs (including PDF),
-- and PNG for anything else.
--
module Text.Pandoc.PlantUML.Filter.Formats(imageFormatTypeFor) where

import Text.Pandoc.Definition
import Text.Pandoc.PlantUML.Filter.Types

-- | The image file type to be used for the given output format.
-- EPS is used for latex outputs, as it provides lossless scalability
-- All other output formats use PNG for now.
imageFormatTypeFor :: Format -> ImageFormat
imageFormatTypeFor (Format "latex") =  "eps"
imageFormatTypeFor _                =  "png"
