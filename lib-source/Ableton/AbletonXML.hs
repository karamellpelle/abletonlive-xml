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
module Ableton.AbletonXML
  (
    AbletonXML (..),
    ToAbletonXML (..),
  ) where

import RIO
import qualified RIO.ByteString as B 
import qualified RIO.Text as T
import Text.XML.Light

import Ableton.AbletonData

-- | data holding XML data
data AbletonXML = 
    AbletonXML
    {
        abletonxmlText :: T.Text
    }

--------------------------------------------------------------------------------
--  ToAbletonXML

-- | convert a type  to an `AbletonXML`
class ToAbletonXML a where
    toAbletonXML :: a -> Maybe AbletonXML



-- | `AbletonXML -> AbletonXML`
instance ToAbletonXML AbletonXML where
    toAbletonXML = Just

--------------------------------------------------------------------------------
--  peek AbletonDataType from XML data

instance AbletonData AbletonXML where
    abletondataType axml = 
        undefined
        --case parseXMLDoc (abletonxmlData axml) of
        --    Nothing   -> error "XML is not AbletonData"
        --    Just doc  -> undefined

--Ableton > InstrumentVector
--Ableton > LiveSet
--Ableton > GroupDevicePreset

        -- ^ FIXME: look at XML definition
{-

    case abletonxmlType axml of
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
