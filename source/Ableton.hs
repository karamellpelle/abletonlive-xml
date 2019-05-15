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
    readfileAbletonXML,
    writefileAbletonXML,

    readfileAbletonBin,
    writefileAbletonBin,

    createXML, -- tmp
  ) where

import qualified Data.ByteString.Lazy as BS
import System.EasyFile
import Data.Maybe

import Ableton.AbletonData
import Ableton.AbletonFile
import Ableton.AbletonXML
import Ableton.AbletonBin
import Ableton.Convert
--------------------------------------------------------------------------------
--  read & write AbletonXML

-- | TODO: 
--readfileAbleton :: (ToAbletonFile a) => FilePath -> IO (AbletonFile a)
--readfileAbleton path = do
--  
--
--readfileAbleton file :: FilePath -> IO (AbletonFile AbletonXML)
    

-- | no verification of file content; it assumes correct XML definition
--   TODO: verify by filepath extension?
readfileAbletonXML :: FilePath -> IO (AbletonFile AbletonXML)
readfileAbletonXML path = do
    content <- BS.readFile path
    return $ AbletonFile path $ AbletonXML { abletonxmlContent = content }
    

writefileAbletonXML :: AbletonFile AbletonXML -> IO FilePath
writefileAbletonXML file = do
    BS.writeFile (abletonfilePath file) $ abletonxmlContent $ abletonfile file
    return $ abletonfilePath file


--------------------------------------------------------------------------------
--  read & write AbletonFile

-- | no verification of file content; it assumes correct extension

readfileAbletonBin :: FilePath -> IO (AbletonFile AbletonBin)
readfileAbletonBin path = do
    content <- BS.readFile path
    return $ AbletonFile path $ AbletonBin
                                {
                                    abletonbinType = fromJust $ extToAbletonData $ takeExtensions path, -- no verification done!
                                    abletonbinContent = content
                                }


writefileAbletonBin :: (AbletonFile AbletonBin) -> IO FilePath
writefileAbletonBin file = do
    BS.writeFile (abletonfilePath file) $ abletonbinContent $ abletonfile file
    return $ abletonfilePath file


createXML :: FilePath -> IO FilePath
createXML path = do
    file <- readfileAbletonBin path
    writefileAbletonXML $ changeExtXML $ AbletonFile path (toAbletonXML file)

        where
          changeExtXML = changeAbletonFilePath $ \path -> addExtension path ".xml"
              --addExtension path ".xml" -- adding .xml to current extension, i.e. file.adg -> file.adg.xml

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
