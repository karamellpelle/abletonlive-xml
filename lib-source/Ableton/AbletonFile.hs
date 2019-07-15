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
module Ableton.AbletonFile
  (
    AbletonFile (..),
    
    readAbletonFileXML,
    writeAbletonFileXML,
    readAbletonFileBin,
    writeAbletonFileBin,

    abletonfileToXML,
    abletonfileToBin,

    filepathIsAbletonBin,
    filepathIsAbletonXML,

  ) where

import RIO
import RIO.FilePath
import RIO.Directory
import qualified RIO.ByteString as B
import qualified RIO.Char as C


import MyPrelude
import Ableton.AbletonData
import Ableton.AbletonBin
import Ableton.AbletonXML



-- | a file with Ableton data
data AbletonFile a = 
    AbletonFile
    {
        abletonfileFilePath :: FilePath,
        abletonfileData :: a
    }
-- TODO: AbletonFile = forall b. AbletonData b => abletonfileData

--------------------------------------------------------------------------------
--  read write Bin

readAbletonFileBin :: MonadUnliftIO m => FilePath -> m (Either String (AbletonFile AbletonBin))
readAbletonFileBin path = do
    case filepathToAbletonData path of
        Nothing -> pure $ Left $ "based on extension, file is not an AbletonFile"
        Just t  -> do
            bin <- tryIO $ liftIO $ readFileBinary path -- try to read file as a ByteString
            case bin of
                Left exc    -> pure $ Left $ displayException exc
                Right bin'  -> pure $ Right $ AbletonFile path $ AbletonBin t bin'
            
        
    

writeAbletonFileBin :: MonadUnliftIO m => AbletonFile AbletonBin -> m (Either String FilePath)
writeAbletonFileBin file = do
    a <- tryIO $ writeFileBinary (abletonfileFilePath file) (abletonbinData $ abletonfileData file)
    case a of
        Left exc    -> pure $ Left $ displayException exc
        Right _     -> pure $ Right $ abletonfileFilePath file
--------------------------------------------------------------------------------
--  read write XML

readAbletonFileXML :: MonadUnliftIO m => FilePath -> m (Either Text (AbletonFile AbletonXML))
readAbletonFileXML path = do
    text <- tryIO $ readFileUtf8 path
    case text of
        Left exc    -> pure $ Left $ textDisplay exc
        Right text' -> pure $ Right $ AbletonFile path $ AbletonXML text'
    

writeAbletonFileXML :: MonadUnliftIO m => AbletonFile AbletonXML -> m (Either Text FilePath)
writeAbletonFileXML file = do
    a <- tryIO $ writeFileUtf8 (abletonfileFilePath file) (abletonxmlText $ abletonfileData file)
    case a of
        Left exc    -> pure $ Left $ textDisplay exc
        Right _     -> pure $ Right $ abletonfileFilePath file

--------------------------------------------------------------------------------
--  convert 

-- | convert to XML.
abletonfileToXML :: ToAbletonXML a => AbletonFile a -> Maybe (AbletonFile AbletonXML)
abletonfileToXML file =
    case toAbletonXML $ abletonfileData file of
        Nothing   -> Nothing
        Just xml  -> Just $ AbletonFile
                     {
                        abletonfileFilePath = filepathToXML (abletondataType xml) $ abletonfileFilePath file,
                        abletonfileData     = xml
                     }


-- | convert to Bin
abletonfileToBin :: ToAbletonBin a => AbletonFile a -> Maybe (AbletonFile AbletonBin)
abletonfileToBin file =
    case toAbletonBin $ abletonfileData file of
        Nothing   -> Nothing
        Just bin  -> Just $ AbletonFile
                     {
                        abletonfileFilePath = filepathToBin (abletondataType bin) $ abletonfileFilePath file,
                        abletonfileData     = bin
                     }

--------------------------------------------------------------------------------
--  file extensions

filepathToXML :: AbletonDataType -> FilePath -> FilePath
filepathToXML datatype path = 
    dropExtensions path <.> abletondataXMLExt datatype
    
filepathToBin :: AbletonDataType -> FilePath -> FilePath
filepathToBin datatype path = 
    dropExtensions path <.> abletondataBinExt datatype


-- | what kind of AbletonData holds this file?
extToAbletonData :: String -> Maybe AbletonDataType
extToAbletonData ext = case fmap C.toLower ext of -- uppercase == lowercase
      ".adg"  -> Just DataADG
      ".agr"  -> Just DataAGR
      ".adv"  -> Just DataADV
      ".alc"  -> Just DataALC
      ".als"  -> Just DataALS
      ".alp"  -> Just DataALP
      ".ams"  -> Just DataAMS
      ".amxd" -> Just DataAMXD
      ".asd"  -> Just DataASD
      ".asx"  -> Just DataASX
      _       -> Nothing

filepathToAbletonData :: FilePath -> Maybe AbletonDataType
filepathToAbletonData =
    extToAbletonData . takeExtensions . removeXML
    where
      removeXML []                     = []
      removeXML ('.' : 'x':'m':'l':[]) = []
      removeXML (a:as)                 = a : removeXML as
      -- ^ does not convert to lower case!

-- | get XML extension from AbletonDataType
abletondataXMLExt :: AbletonDataType -> String 
abletondataXMLExt t = 
    abletondataXMLExt t <.> "xml"
    
    
-- | get Bin extension from AbletonDataType
abletondataBinExt :: AbletonDataType -> String 
abletondataBinExt t = case t of 
    DataADG -> ".adg"
    DataAGR -> ".agr"
    DataADV -> ".adv"
    DataALC -> ".alc"
    DataALS -> ".als"
    DataALP -> ".alp"
    DataAMS -> ".ams"
    DataAMXD -> ".amxd"
    DataASD -> ".asd"
    DataASX -> ".asx"
    
-- | known extensions of Ableton binary files
abletonfileBinExts :: [String]
abletonfileBinExts = [
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

-- | known extensions of Ableton xmlary files
abletonfileXMLExts :: [String]
abletonfileXMLExts =
    fmap (<.> "xml") abletonfileBinExts

-- | is filepath a binary file of Ableton?
filepathIsAbletonBin :: FilePath -> Bool
filepathIsAbletonBin path =
    elem (takeExtensions path) abletonfileBinExts

-- | is filepath a XML file of Ableton?
filepathIsAbletonXML :: FilePath -> Bool
filepathIsAbletonXML path =
    elem (takeExtensions path) abletonfileXMLExts


--------------------------------------------------------------------------------
--  path operations
{-
modifyAbletonFilePath :: (FilePath -> FilePath) -> AbletonFile a -> AbletonFile a
modifyAbletonFilePath f file =
    file { abletonfileFilePath = f $ abletonfileFilePath file }


-- | change root folder, for example
--
--      changeAbletonFileRoot "xml/" "../" "xml/Packs/HiTech/Bass/CoolB.adg.xml" == "../Packs/HiTech/Bass/CoolB.adg.xml"
--
--      changeAbletonFileRoot "" "xml/" "Packs/HiTech/Bass/CoolB.adg" == "xml/Packs/HiTech/Bass/CoolB.adg"
--
changeAbletonFileRoot :: FilePath -> FilePath -> AbletonFile a -> AbletonFile a
changeAbletonFileRoot root root' = 
    modifyAbletonFilePath $ \path ->
        case stripPrefix root path of
            Just path' -> root' </> path'
            Nothing    -> path
        -- ^ TODO: work with normalized paths. 
        --         and think thourg what happens if a path is absolute, cf. semantics of '</>'

changeAbletonFileBaseName :: String -> AbletonFile a -> AbletonFile a
changeAbletonFileBaseName name = 
    modifyAbletonFilePath $ \path -> replaceBaseName path name
-}
    
