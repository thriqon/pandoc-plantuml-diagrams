{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module          : Text.Pandoc.PlantUML.Filter.FileNameGenerator
-- Description     : Generate a filename only depending on the contents of it.
-- Copyright       : (c) Jonas Weber, 2015
-- License         : ISC
--
-- This package does its best to avoid rerendering the same diagrams (as in, with
-- the same source) if not neccessary. It uses a cryptographic hash (namely SHA1)
-- to get a stable identifier for the contents.
--
--
module Text.Pandoc.PlantUML.Filter.FileNameGenerator(fileNameForSource) where

import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Text as T
import Text.Pandoc.PlantUML.Filter.Types

-- | Generates the Hash of a diagram source, and prefixes that.
fileNameForSource :: DiagramSource -> ImageName
fileNameForSource (DiagramSource source) = prefix <> (hash source)
  where hash = T.pack . showDigest . sha1 . fromString . T.unpack

-- | The prefix to put before rendered images
prefix :: T.Text
prefix = ".rendered."
