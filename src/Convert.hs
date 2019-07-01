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
module Convert
  (
    ToAbletonBin (..),
    ToAbletonXML (..),


  ) where

import RIO
import MyPrelude
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import qualified Codec.Compression.GZip as GZip

import Ableton.AbletonData
import Ableton.AbletonBin
import Ableton.AbletonXML
import Ableton.AbletonFile


--------------------------------------------------------------------------------
--  ToAbletonBin

-- | convert a type  to an `AbletonBin`
--   TODO: Maybe
class ToAbletonBin a where
    toAbletonBin :: a -> Maybe AbletonBin



-- | `AbletonBin -> AbletonBin`
instance ToAbletonBin AbletonBin where
    toAbletonBin = Just


-- | `AbletonXML -> AbletonBin`
--   thi application is based upon this function
instance ToAbletonBin AbletonXML where
    toAbletonBin axml = 
        Just AbletonBin
        { 
            abletonbinType = abletondataType axml,               -- assuming valid XML, no verificationD of valid XML
            abletonbinData = BL.toStrict $ GZip.compress $ BL.fromStrict $ abletonxmlData axml -- NOTE use 'compressWith' to control compression parameters
        }


instance (ToAbletonBin a) => ToAbletonBin (AbletonFile a) where
    toAbletonBin file = toAbletonBin $ abletonfileContent file


--------------------------------------------------------------------------------
--  ToAbletonXML

-- | convert a type  to an `AbletonXML`
class ToAbletonXML a where
    toAbletonXML :: a -> Maybe AbletonXML



-- | `AbletonXML -> AbletonXML`
instance ToAbletonXML AbletonXML where
    toAbletonXML = Just

-- | `AbletonBin -> AbletonXML`
--   this application is based upon this function
instance ToAbletonXML AbletonBin where
    toAbletonXML abin = 
        Just AbletonXML
        {
            abletonxmlData =  BL.toStrict $ GZip.decompress $ BL.fromStrict $ abletonbinData abin
        }

-- | file to AbletonXML 
instance (ToAbletonXML a) => ToAbletonXML (AbletonFile a) where
    toAbletonXML file = toAbletonXML $ abletonfileContent file



