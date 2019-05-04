-- abletonlive-xml : Ableton Live files as XML
-- Copyright (C) 2019 karamellpelle@hotmail.com
-- 
-- This file is part of abletonlive-xml.
-- 
-- grid is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- grid is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with grid.  If not, see <http://www.gnu.org/licenses/>.
--
module Ableton
  (
    readAbletonXML,
    writeAbletonXML,
    readAbletonFile,
    writeAbletonFile
  ) where

import qualified Data.ByteString.Lazy as BS
import System.EasyFile
import Data.Maybe

import Ableton.AbletonFileType
import Ableton.AbletonFile
import Ableton.AbletonXML
import Ableton.Convert
--------------------------------------------------------------------------------
--  read & write AbletonXML


-- | no verification of file content; it assumes correct XML definition
readAbletonXML :: FilePath -> IO AbletonXML
readAbletonXML path = do
    content <- BS.readFile path
    return $ AbletonXML
             {
                abletonxmlPath = path,
                abletonxmlContent = content
             }
    

writeAbletonXML :: AbletonXML -> IO FilePath
writeAbletonXML axml = do
    BS.writeFile (abletonxmlPath axml) $ abletonxmlContent axml
    return $ abletonxmlPath axml


--------------------------------------------------------------------------------
--  read & write AbletonFile

-- | no verification of file content; it assumes correct extension
readAbletonFile :: FilePath -> IO AbletonFile
readAbletonFile path = do
    content <- BS.readFile path
    return $  AbletonFile
              {
                abletonfilePath = path,
                abletonfileType = fromJust $ extensionToAbletonFileType $ takeExtensions path,
                abletonfileContent = content
              }


writeAbletonFile :: AbletonFile -> IO FilePath
writeAbletonFile afile = do
    BS.writeFile (abletonfilePath afile) $ abletonfileContent afile
    return $ abletonfilePath afile
    


--copyToAbletonFile :: FilePath -> IO FilePath
--copyToAbletonFile path dir = do
--    undefined
    --when (isAbletonFile path) $ 
    --when (isAbletonXML path) $ 
    -- read file type

{-
writeAbletonXML =<< (fmap toAbletonXML) readAbletonXML


    (fmap toAbletonFile $ readAbletonXML path) >>= writeAbletonFile
    
    (a -> b) -> (a -> m a) -> a -> m b
readAbletonXML > toAbletonFile > writeAbletonFile

--changeRoot "/a/share" "/b/myableton" $ toAbletonFile axml
-}
