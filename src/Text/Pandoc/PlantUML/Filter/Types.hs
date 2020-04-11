
-- |
-- Module          : Text.Pandoc.PlantUML.Filter.Types
-- Description     : Defines the common types used in this package
-- Copyright       : (c) Jonas Weber, 2015
-- License         : ISC
--
module Text.Pandoc.PlantUML.Filter.Types where
import qualified Data.Text as T

-- | The name of an image, without extension, usually a hash
type ImageName = T.Text

-- | The source of a diagram
newtype DiagramSource = DiagramSource T.Text deriving (Eq, Show)

-- | An image format, e.g. "eps"
type ImageFormat = T.Text

-- | A filename of an image. It contains the basename (myawesomepicture) and
-- the extension (jpg). It can be shown, which is basically
-- "myawesomepicture.jpg"
data ImageFileName = ImageFileName ImageName ImageFormat deriving Eq

-- | Show the image file name by joining basename and extension with
-- a dot, yielding picture.jpg
instance Show ImageFileName where
  show (ImageFileName name format) = (T.unpack name) ++ "." ++ (T.unpack format)

-- | External impure actions are encapsulated in this monad.
class Monad m => ImageIO m where
  -- | Tells whether an image with the given file name
  -- is already present in the store (e.g., the filesystem).
  doesImageExist     :: ImageFileName -> m Bool
  -- | Calls out to an external diagram processor (PlantUML)
  -- to render the source to the given image file name.
  renderImage        :: ImageFileName -> DiagramSource -> m ()
