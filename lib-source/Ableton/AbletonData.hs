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
module Ableton.AbletonData
  (
    AbletonDataType (..),
    AbletonData (..),

  ) where

import RIO

-- | https://help.ableton.com/hc/en-us/articles/209769625-Live-specific-file-types-adg-als-alp-
data AbletonDataType = 
    DataADG | -- ^ device group
    DataAGR | -- ^ groove file. NOT GZIP XML
    DataADV | -- ^ device preset
    DataALC | -- ^ Live clip
    DataALS | -- ^ Live set
    DataALP | -- ^ Live pack. NOT GZIP XML
    DataAMS | -- ^ meta sound
    DataAMXD | -- ^ max for Live. NOT GZIP XML
    DataASD | -- ^ warp analysis. NOT GZIP XML
    DataASX   -- ^ skin file
    deriving (Show)
    

-- | retrieve kind of Ableton data from type
class AbletonData a where
    abletondataType :: a -> AbletonDataType


