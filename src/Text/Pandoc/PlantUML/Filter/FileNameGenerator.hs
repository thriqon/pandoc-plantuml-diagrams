
-- | Module : Text.Pandoc.PlantUML.Filter.FileNameGenerator
-- Generate a filename only depending on the contents of it.
module Text.Pandoc.PlantUML.Filter.FileNameGenerator(fileNameForSource) where

import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.ByteString.Lazy.UTF8 (fromString)

import Text.Pandoc.PlantUML.Filter.Types

-- | Generates the Hash of a diagram source, and prefixes that.
fileNameForSource :: DiagramSource -> ImageName
fileNameForSource source = prefix ++ (hash source)
  where hash = showDigest . sha1 . fromString

-- | The prefix to put before rendered images
prefix :: String
prefix = ".rendered."
