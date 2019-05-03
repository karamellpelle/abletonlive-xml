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
module Ableton.AbletonFileType
  (
    AbletonFileType (..),
    extensionToAbeltonFileType,
    abletonfiletypeToExtension
  ) where

import Data.Char


-- | https://help.ableton.com/hc/en-us/articles/209769625-Live-specific-file-types-adg-als-alp-
data AbletonFileType = 
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
    

--------------------------------------------------------------------------------
--  

extensionToAbeltonFileType :: String -> Maybe AbletonFileType
extensionToAbeltonFileType ext = case fmap toLower ext of -- make sure case does not matter
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

abletonfiletypeToExtension :: AbletonFileType -> String 
abletonfiletypeToExtension t = case t of 
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
    
