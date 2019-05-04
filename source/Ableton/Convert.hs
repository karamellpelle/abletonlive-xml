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
--{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE FlexibleInstances #-}
module Ableton.Convert
  (
    ModifyPath (..),
    changeRoot,
    changeBaseName,
    
    ToAbletonFile (..),
    ToAbletonXML (..),


  ) where

import Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import System.EasyFile
import Data.List
import Data.Maybe

import Ableton.AbletonFileType
import Ableton.AbletonFile
import Ableton.AbletonXML

--------------------------------------------------------------------------------
--  modify path

class ModifyPath a where
    modifyPath :: (FilePath -> FilePath) -> a -> a
    -- ^ 

instance ModifyPath AbletonFile where
    modifyPath f afile = 
        afile { abletonfilePath = f $ abletonfilePath afile }

instance ModifyPath AbletonXML where
    modifyPath f axml = 
        axml { abletonxmlPath = f $ abletonxmlPath axml }

--instance ModifyPath FilePath where
--    modifyPath f = f



-- | change root folder, for example
--
--      changeRoot "xml/" "../" "xml/Packs/HiTech/Bass/CoolB.adg.xml" == "../Packs/HiTech/Bass/CoolB.adg.xml"
--
--      changeRoot "" "xml/" "Packs/HiTech/Bass/CoolB.adg" == "xml/Packs/HiTech/Bass/CoolB.adg"
--
-- | TODO: work with normalized paths. 
--         and think thourg what happens if a path is absolute, cf. semantics of '</>'
changeRoot :: ModifyPath a => FilePath -> FilePath -> a -> a
changeRoot root root' = modifyPath helper
    where
      helper = \path -> 
          case stripPrefix root path of
              Just path' -> root' </> path'
              Nothing    -> path

changeBaseName :: ModifyPath a => String -> a -> a
changeBaseName name = modifyPath helper
    where
      helper = \path -> replaceBaseName path name


--------------------------------------------------------------------------------
--  ToAbletonFile

-- | convert a type (typically file data) to an `AbletonFile`
--   TODO: Maybe
class ToAbletonFile a where
    toAbletonFile :: a -> AbletonFile



-- | `AbletonFile -> AbletonFile`
instance ToAbletonFile AbletonFile where
    toAbletonFile = id

-- | `AbletonXML -> AbletonFile`
instance ToAbletonFile AbletonXML where
    toAbletonFile axml = 
        AbletonFile
        { 
            abletonfilePath = changePath $ abletonxmlPath axml,
            abletonfileType = filetype,
            abletonfileContent = GZip.compress $ abletonxmlContent axml -- use compressWith for control over compression parameters
        }
        where
          filetype = fromJust $ abletonxmlType axml -- assuming valid XML, no verificationD of valid XML
          changePath path = 
              let (path', ext) = splitExtensions path
              in  path' <.> abletonfiletypeToExtension filetype




--------------------------------------------------------------------------------
--  ToAbletonXML

-- | convert a type (typically file data) to an `AbletonXML`
class ToAbletonXML a where
    toAbletonXML :: a -> AbletonXML


-- | `AbletonFile -> AbletonXML`
instance ToAbletonXML AbletonXML where
    toAbletonXML = id

-- | `AbletonFile -> AbletonXML`
instance ToAbletonXML AbletonFile where
    toAbletonXML afile = 
        AbletonXML
        {
            abletonxmlPath = changePath $ abletonfilePath afile,
            abletonxmlContent =  GZip.decompress $ abletonfileContent afile
        }
        where
          changePath path =
              addExtension path ".xml" -- just add .xml to current extension, i.e. file.adg -> file.adg.xml


--------------------------------------------------------------------------------
--  peek AbletonFileType from XML data

abletonxmlType :: AbletonXML -> Maybe AbletonFileType
abletonxmlType xml = 
    undefined
{-

    case abletonxmlType axml of
    uKk
        FileADG | -- ^ device group
        FileAGR | -- ^ groove file
        FileADV | -- ^ device preset
        FileALC | -- ^ Live clip
        FileALS | -- ^ Live set
        FileALP | -- ^ Live pack
        FileAMS | -- ^ meta sound
        FileAMXD | -- ^ max for Live
        FileASD | -- ^ warp analysis
        FileASX   -- ^ skin file
        
    undefined
-}
-- ^ FIXME: look at XML definition
