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
    abletonfilebinExtensions,
    filepathIsAbletonBin,
    filepathIsAbletonXML,


    -- XML
    readfileAbletonXML,
    writefileAbletonXML,
    -- Bin
    readAbletonFileBin,
    writeAbletonFileBin,

    createXML, -- tmp
  ) where

import qualified Data.ByteString.Lazy as BS
import System.EasyFile
import Data.Maybe
import Data.Char

import Ableton.AbletonData
import Ableton.AbletonFile
import Ableton.AbletonXML
import Ableton.AbletonBin
import Ableton.Convert


--------------------------------------------------------------------------------
--  file names

--------------------------------------------------------------------------------
--  read & write AbletonXML

-- | TODO: 
--readfileAbleton :: (ToAbletonFile a) => FilePath -> IO (AbletonFile a)
--readfileAbleton path = do
--  
--
--readfileAbleton file :: FilePath -> IO (AbletonFile AbletonXML)
    

-- | no verification of file dat; it assumes correct XML definition
--   TODO: verify by filepath extension?
readfileAbletonXML :: FilePath -> IO (AbletonFile AbletonXML)
readfileAbletonXML path = do
    dat <- BS.readFile path
    return $ AbletonFile path $ AbletonXML { abletonxmlData = dat }
    

writefileAbletonXML :: AbletonFile AbletonXML -> IO FilePath
writefileAbletonXML file = do
    BS.writeFile (abletonfilePath file) $ abletonxmlData $ abletonfileContent file
    return $ abletonfilePath file


--------------------------------------------------------------------------------
--  read & write AbletonFile

-- | no verification of file dat; it assumes correct extension

readAbletonFileBin :: FilePath -> IO (AbletonFile AbletonBin)
readAbletonFileBin path = do
    dat <- BS.readFile path
    return $ AbletonFile path $ AbletonBin
                                {
                                    abletonbinType = fromJust $ extToAbletonData $ takeExtensions path, -- no verification done!
                                    abletonbinData = dat
                                }


writeAbletonFileBin :: (AbletonFile AbletonBin) -> IO FilePath
writeAbletonFileBin file = do
    BS.writeFile (abletonfilePath file) $ abletonbinData $ abletonfileContent file
    return $ abletonfilePath file



-- | known extensions of Ableton binary files
abletonfilebinExtensions :: [String]
abletonfilebinExtensions = [
      ".adg",
      ".agr",
      ".adv",
      ".alc",
      ".als",
      ".alp",
      ".ams",
      ".amxd",
      ".asd",
      ".asx" ]

-- | is file(path) a binary file of Ableton?
filepathIsAbletonBin :: FilePath -> Bool
filepathIsAbletonBin path =
    elem (takeExtension path) abletonfilebinExtensions

-- | is file(path) a XML file of Ableton?
filepathIsAbletonXML :: FilePath -> Bool
filepathIsAbletonXML path =
    elem (takeExtension path) [".xml"]



-- | use extension to find what kind of Ableton data
extToAbletonData :: String -> Maybe AbletonDataType
extToAbletonData ext = case fmap toLower ext of -- uppercase == lowercase
      ".adg" -> Just FileADG 
      ".agr" -> Just FileAGR
      ".adv" -> Just FileADV
      ".alc" -> Just FileALC
      ".als" -> Just FileALS
      ".alp" -> Just FileALP
      ".ams" -> Just FileAMS
      ".amxd" -> Just FileAMXD
      ".asd" -> Just FileASD
      ".asx" -> Just FileASX
      _     -> Nothing

-- | get extension from AbletonDataType
abletondataToExt :: AbletonDataType -> String 
abletondataToExt t = case t of 
    FileADG -> ".adg"
    FileAGR -> ".agr"
    FileADV -> ".adv"
    FileALC -> ".alc"
    FileALS -> ".als"
    FileALP -> ".alp"
    FileAMS -> ".ams"
    FileAMXD -> ".amxd"
    FileASD -> ".asd"
    FileASX -> ".asx"
    


--------------------------------------------------------------------------------
--  find Ableton files

{-

-- | find Ableton XML files based on a list of files and folders
findFilePathsAbletonXML :: [FilePath] -> IO [FilePath]
findFilePathsAbletonXML = findFilePaths filepathIsAbletonXML

findFilePathsAbletonBin :: [FilePath] -> IO [FilePath]
findFilePathsAbletonBin = findFilePaths filepathIsAbletonBin

findFilePaths :: (FilePath -> Bool) -> [FilePath] -> IO [FilePath]
findFilePaths pred paths =
    fmap concat
    case path of 
        folder -> mapM findFilePathsXML (++) [] foldercontent
        file  -> [file]
            case extension of 
                ".xml"  -> path
                _ -> []
        _ -> return [] -- should not happen

readAbletonXMLs:: [FilePath] -> IO (AbletonFile AbletonXML)
readAbletonXMLs = findFilePathsXML >>= readAbletonFileXMLS


findFilePathsXML :: [] 
    files <- case fileargs of 
        [] -> findAbletonFileBin
class ToAbletonXML a where
    toAbletonXML :: a -> Maybe AbletonXML 
-}
--------------------------------------------------------------------------------
--  

createXML :: FilePath -> IO FilePath
createXML path = do
    file <- readAbletonFileBin path
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

