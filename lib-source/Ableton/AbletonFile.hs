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

    extToAbletonData,
    abletondataToBinExt,
    filepathIsAbletonBin,
    filepathIsAbletonXML,

    -- TODO: remove
    modifyAbletonFilePath,
    --changeAbletonFileRoot,
    --changeAbletonFileBaseName,

  ) where

import RIO
import qualified RIO.ByteString as B
import qualified RIO.Char as C

import System.EasyFile

import MyPrelude
import Ableton.AbletonData



-- | use GADTs?
data AbletonFile a = 
    AbletonFile
    {
        abletonfilePath :: FilePath,
        abletonfileData :: a
    }
-- TODO: AbletonFile = forall b. AbletonData b => abletonfileData

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

-- | get extension from AbletonDataType
abletondataToBinExt :: AbletonDataType -> String 
abletondataToBinExt t = case t of 
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
    elem (takeExtension path) [".xml"]


--------------------------------------------------------------------------------
--  path operations

modifyAbletonFilePath :: (FilePath -> FilePath) -> AbletonFile a -> AbletonFile a
modifyAbletonFilePath f file =
    file { abletonfilePath = f $ abletonfilePath file }

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
    
