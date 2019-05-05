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
    abletondataToExt,

    changeAbletonFilePath,
    changeAbletonFileRoot,
    changeAbletonFileBaseName,

  ) where

import MyPrelude
import System.EasyFile
import Data.Char

import Ableton.AbletonData



data AbletonFile a = 
    AbletonFile
    {
        abletonfilePath :: FilePath,
        abletonfile :: a
    }


--------------------------------------------------------------------------------
--  path operations

changeAbletonFilePath :: (FilePath -> FilePath) -> AbletonFile a -> AbletonFile a
changeAbletonFilePath f file =
    file { abletonfilePath = f $ abletonfilePath file }


-- | change root folder, for example
--
--      changeAbletonFileRoot "xml/" "../" "xml/Packs/HiTech/Bass/CoolB.adg.xml" == "../Packs/HiTech/Bass/CoolB.adg.xml"
--
--      changeAbletonFileRoot "" "xml/" "Packs/HiTech/Bass/CoolB.adg" == "xml/Packs/HiTech/Bass/CoolB.adg"
--
changeAbletonFileRoot :: FilePath -> FilePath -> AbletonFile a -> AbletonFile a
changeAbletonFileRoot root root' = 
    changeAbletonFilePath $ \path ->
        case stripPrefix root path of
            Just path' -> root' </> path'
            Nothing    -> path
        -- ^ TODO: work with normalized paths. 
        --         and think thourg what happens if a path is absolute, cf. semantics of '</>'

changeAbletonFileBaseName :: String -> AbletonFile a -> AbletonFile a
changeAbletonFileBaseName name = 
    changeAbletonFilePath $ \path -> replaceBaseName path name

    
--------------------------------------------------------------------------------
--  

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
    

