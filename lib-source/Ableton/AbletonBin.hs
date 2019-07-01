-- abletonlive-xml : Ableton Live bins as XML
-- Copyright (C) 2019 karamellpelle@hotmail.com
-- 
-- This bin is part of abletonlive-xml.
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
module Ableton.AbletonBin
  (
    AbletonBin (..),
    ToAbletonBin (..),
  ) where

import RIO
import MyPrelude

import Ableton.AbletonData
import qualified RIO.ByteString as B -- "don't use Lazy - https://haskell.fpcomplete.com/tutorial/string-types"


-- | data holding binary data
data AbletonBin = 
    AbletonBin
    {
        abletonbinType :: AbletonDataType,
        abletonbinData :: B.ByteString
    }


--------------------------------------------------------------------------------
--  ToAbletonBin

-- | convert a type  to an `AbletonBin`
--   TODO: Maybe
class ToAbletonBin a where
    toAbletonBin :: a -> Maybe AbletonBin



-- | `AbletonBin -> AbletonBin`
instance ToAbletonBin AbletonBin where
    toAbletonBin = Just



