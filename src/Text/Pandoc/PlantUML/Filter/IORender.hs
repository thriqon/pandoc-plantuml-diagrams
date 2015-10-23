
-- | Module: Text.Pandoc.PlantUML.Filter.Render
-- Defines the actual rendering done with PlantUML
module Text.Pandoc.PlantUML.Filter.IORender() where

import System.IO (hClose, hPutStr, IOMode(..), withBinaryFile, Handle)
import Data.ByteString.Lazy (hGetContents, hPut)
import System.Process
import System.Directory

import Text.Pandoc.PlantUML.Filter.Types

instance ImageIO IO where
  renderImage imageFileName source = do
    (Just hIn, Just hOut, _, _) <- createProcess $ plantUmlProcess imageFileName
    hPutStr hIn source
    hClose hIn
    --img <- hGetContents hOut
    withImageFile $ pipe hOut
    --(\h -> hPut h img)
    hClose hOut
      where withImageFile = withBinaryFile (show imageFileName) WriteMode
  doesImageExist imageFileName = doesFileExist $ show imageFileName

plantUmlProcess :: ImageFileName -> CreateProcess
plantUmlProcess (ImageFileName _ fileType) = (proc "java" ["-jar", "plantuml.jar", "-pipe", "-t" ++ fileType])
  { std_in = CreatePipe, std_out = CreatePipe }

pipe :: Handle -> Handle -> IO ()
pipe hIn hOut = do
  input <- hGetContents hIn
  hPut hOut input

