{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module: Text.Pandoc.PlantUML.Filter.Render
-- Defines the actual rendering done with PlantUML
module Text.Pandoc.PlantUML.Filter.IORender() where

import System.IO (hClose, IOMode(..), withBinaryFile, Handle)
import Data.Text.IO (hPutStr)
import qualified Data.Text as T
import Data.ByteString.Lazy (hGetContents, hPut)
import System.Process
import System.Directory

import Text.Pandoc.PlantUML.Filter.Types

instance ImageIO IO where
  renderImage imageFileName (DiagramSource source) = do
    (Just hIn, Just hOut, _, _) <- createProcess $ plantUmlProcess imageFileName
    hPutStr hIn source
    hClose hIn
    withImageFile $ pipe hOut
    hClose hOut
      where withImageFile = withBinaryFile (show imageFileName) WriteMode
  doesImageExist imageFileName = doesFileExist $ show imageFileName

plantUmlProcess :: ImageFileName -> CreateProcess
plantUmlProcess (ImageFileName _ fileType) = (proc "java" ["-jar", "plantuml.jar", "-pipe", "-t" ++ (T.unpack fileType)])
  { std_in = CreatePipe, std_out = CreatePipe }

pipe :: Handle -> Handle -> IO ()
pipe hIn hOut = do
  !input <- hGetContents hIn
  hPut hOut input

