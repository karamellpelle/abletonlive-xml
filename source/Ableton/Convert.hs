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
    ToAbletonBin (..),
    ToAbletonXML (..),


  ) where

import Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Maybe

import Ableton.AbletonData
import Ableton.AbletonBin
import Ableton.AbletonXML
import Ableton.AbletonFile


--------------------------------------------------------------------------------
--  ToAbletonBin

-- | convert a type  to an `AbletonBin`
--   TODO: Maybe
class ToAbletonBin a where
    toAbletonBin :: a -> AbletonBin



-- | `AbletonBin -> AbletonBin`
instance ToAbletonBin AbletonBin where
    toAbletonBin = id


-- | `AbletonXML -> AbletonBin`
instance ToAbletonBin AbletonXML where
    toAbletonBin axml = 
        AbletonBin
        { 
            abletonbinType = abletondataType axml,-- assuming valid XML, no verificationD of valid XML
            abletonbinData = GZip.compress $ abletonxmlData axml -- use compressWith for control over compression parameters
        }


instance (ToAbletonBin a) => ToAbletonBin (AbletonFile a) where
    toAbletonBin file = toAbletonBin $ abletonfileContent file


          --filetype = fromJust $ abletonxmlType axml 
          --changePath path = 
          --    let (path', ext) = splitExtensions path
          --    in  path' <.> abletonbintypeToExtension filetype
--------------------------------------------------------------------------------
--  ToAbletonXML

-- | convert a type  to an `AbletonXML`
class ToAbletonXML a where
    toAbletonXML :: a -> AbletonXML



-- | `AbletonXML -> AbletonXML`
instance ToAbletonXML AbletonXML where
    toAbletonXML = id

-- | `AbletonBin -> AbletonXML`
instance ToAbletonXML AbletonBin where
    toAbletonXML abin = 
        AbletonXML
        {
            abletonxmlData =  GZip.decompress $ abletonbinData abin
        }

instance (ToAbletonXML a) => ToAbletonXML (AbletonFile a) where
    toAbletonXML file = toAbletonXML $ abletonfileContent file

        --where
        --  changeExt path =
        --      addExtension path ".xml" -- adding .xml to current extension, i.e. file.adg -> file.adg.xml
        --

--------------------------------------------------------------------------------
-- Convert an AbletonFile to AbletonFile AbletonXML

class  ToAbletonFileXML a where
    toAbletonFileXML :: AbletonFile a -> AbletonFile AbletonXML

instance (ToAbletonXML a) => ToAbletonFileXML (AbletonFile a) where
    toAbletonFileXML (AbletonFile path a) =  
        AbletonFile
        {   
            abletonfilePath = path,
            abletonfileContent = toAbletonXML a
        }
    
--------------------------------------------------------------------------------
-- Convert an AbletonFile to AbletonFile AbletonBin

class  ToAbletonFileBin a where
    toAbletonFileBin :: AbletonFile a -> AbletonFile AbletonBin

instance (ToAbletonBin a) => ToAbletonFileBin (AbletonFile a) where
    toAbletonFileBin (AbletonFile path a) = 
        AbletonFile
        {   
            abletonfilePath = path,
            abletonfileContent = toAbletonBin a
        }
    
