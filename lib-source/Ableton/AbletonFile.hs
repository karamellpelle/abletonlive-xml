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
{-# LANGUAGE OverloadedStrings #-}
module Ableton.AbletonFile
  (
    AbletonFile (..),
    
    readAbletonFileXML,
    writeAbletonFileXML,
    readAbletonFileBin,
    writeAbletonFileBin,

    filepathIsAbletonBin,
    filepathIsAbletonXML,
    extToAbletonData,
    filepathToAbletonData,
    abletondataToExt,

    -- TODO: remove
    modifyAbletonFilePath,
    --changeAbletonFileRoot,
    --changeAbletonFileBaseName,

    
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



-- | use GADTs?
data AbletonFile a = 
    AbletonFile
    {
        abletonfileFilePath :: FilePath,
        abletonfileData :: a
    }
-- TODO: AbletonFile = forall b. AbletonData b => abletonfileData

--------------------------------------------------------------------------------
--  read write Bin

readAbletonFileBin :: MonadUnliftIO m => FilePath -> m (Either Text (AbletonFile AbletonBin))
readAbletonFileBin path = do
    case filepathToAbletonData path of
        Nothing -> pure $ Left $ "based on extension, file is not a AbletonFile"
        Just t  -> do
            bin <- tryIO $ liftIO $ readFileBinary path
            case bin of
                Left exc    -> pure $ Left $ textDisplay exc
                Right bin   -> pure $ Right $ AbletonFile path $ AbletonBin t bin
            
        
    

writeAbletonFileBin :: MonadUnliftIO m => AbletonFile AbletonBin -> m (Either Text FilePath)
writeAbletonFileBin file = do
    a <- tryIO $ writeFileBinary (abletonfileFilePath file) (abletonbinData $ abletonfileData file)
    case a of
        Left exc    -> pure $ Left $ textDisplay exc
        Right _     -> pure $ Right $ abletonfileFilePath file
--------------------------------------------------------------------------------
--  read write XML

readAbletonFileXML :: MonadUnliftIO m => FilePath -> m (Either Text (AbletonFile AbletonXML))
readAbletonFileXML path = do
    text <- tryIO $ readFileUtf8 path
    case text of
        Left exc    -> pure $ Left $ textDisplay exc
        Right text  -> pure $ Right $ AbletonFile path $ AbletonXML text
    

writeAbletonFileXML :: MonadUnliftIO m => AbletonFile AbletonXML -> m (Either Text FilePath)
writeAbletonFileXML file = do
    a <- tryIO $ writeFileUtf8 (abletonfileFilePath file) (abletonxmlText $ abletonfileData file)
    case a of
        Left exc    -> pure $ Left $ textDisplay exc
        Right _     -> pure $ Right $ abletonfileFilePath file
--------------------------------------------------------------------------------
--  

-- | what kind of AbletonData holds this file?
extToAbletonData :: String -> Maybe AbletonDataType
extToAbletonData ext = case fmap C.toLower ext of -- uppercase == lowercase
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

filepathToAbletonData :: FilePath -> Maybe AbletonDataType
filepathToAbletonData =
    extToAbletonData . takeExtension

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
    
-- | known extensions of Ableton binary files
abletonfilebinExts :: [String]
abletonfilebinExts = [
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

-- | is filepath a binary file of Ableton?
filepathIsAbletonBin :: FilePath -> Bool
filepathIsAbletonBin path =
    elem (takeExtension path) abletonfilebinExts

-- | is filepath a XML file of Ableton?
filepathIsAbletonXML :: FilePath -> Bool
filepathIsAbletonXML path =
    elem (takeExtensions path) $ map (<.> ".xml") abletonfilebinExts


--------------------------------------------------------------------------------
--  path operations

modifyAbletonFilePath :: (FilePath -> FilePath) -> AbletonFile a -> AbletonFile a
modifyAbletonFilePath f file =
    file { abletonfileFilePath = f $ abletonfileFilePath file }

{-
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
    
