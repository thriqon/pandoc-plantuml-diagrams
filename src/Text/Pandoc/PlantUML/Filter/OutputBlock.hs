
module Text.Pandoc.PlantUML.Filter.OutputBlock(resultBlock) where

import Text.Pandoc.JSON
import Text.Pandoc.PlantUML.Filter.Types
import Data.Maybe

resultBlock :: ImageFileName -> Attr -> Block
resultBlock imageFileName attr = Para $ map (\p -> p imageFileName attr) [imageTag, idTag]

imageTag :: ImageFileName -> Attr -> Inline
imageTag imageFileName attr    = Image (altTagInline attr) ((show imageFileName), "fig:")

idTag :: ImageFileName -> Attr -> Inline
idTag _ (id, _, _)             = Str ("{#" ++ id ++ "}")

altTagInline :: Attr -> [Inline]
altTagInline (_, _, keyValues)
  | isJust altText             = [Str (fromJust altText)]
  | otherwise                  = []
  where altText = lookup "caption" keyValues
