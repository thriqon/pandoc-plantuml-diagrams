
-- | Module: Text.Pandoc.PlantUML.Filter.Types
-- Defines the common types used in this package
module Text.Pandoc.PlantUML.Filter.Types where

-- | The name of an image, without extension, usually a hash
type ImageName = String

-- | The source of a diagram
type DiagramSource = String

-- | An image format, e.g. "eps"
type ImageFormat = String

data ImageFileName = ImageFileName ImageName ImageFormat

instance Show ImageFileName where
  show (ImageFileName name format) = name ++ "." ++ format

class Monad m => ImageIO m where
  doesImageExist     :: ImageFileName -> m Bool
  renderImage        :: ImageFileName -> DiagramSource -> m ()
