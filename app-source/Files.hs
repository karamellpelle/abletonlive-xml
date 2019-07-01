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
module Files
  (

    -- XML
    readAbletonFileXML,
    writeAbletonFileXML,
    -- Bin
    readAbletonFileBin,
    writeAbletonFileBin,

  ) where

import RIO
import qualified RIO.ByteString as B
import qualified RIO.Char as C

import System.EasyFile
--import Data.Maybe
--import Data.Char

import Ableton



--------------------------------------------------------------------------------
--  file names

--------------------------------------------------------------------------------
--  read & write AbletonXML

-- | TODO: 
--readfileAbleton :: (ToAbletonFile a) => FilePath -> IO (AbletonFile a)
--readfileAbleton path = do
--  
--
--readfileAbleton file :: FilePath -> IO (AbletonFile AbletonXML)
    

-- | no verification of file dat; it assumes correct XML definition
--   TODO: verify by filepath extension?
--
readAbletonFileXML :: FilePath -> IO (AbletonFile AbletonXML)
readAbletonFileXML path = do
    dat <- B.readFile path
    return $ AbletonFile path $ AbletonXML { abletonxmlData = dat }
    

writeAbletonFileXML :: AbletonFile AbletonXML -> IO FilePath
writeAbletonFileXML file = do
    B.writeFile (abletonfilePath file) $ abletonxmlData $ abletonfileData file
    return $ abletonfilePath file


--------------------------------------------------------------------------------
--  read & write AbletonFile

-- | no verification of file dat; it assumes correct extension
readAbletonFileBin :: FilePath -> IO (AbletonFile AbletonBin)
readAbletonFileBin path = do
    dat <- B.readFile path
    return $ AbletonFile path $ AbletonBin
             {
                 --abletonbinType = fromJust $ extToAbletonData $ takeExtensions path, -- TODO: do verification, not isJust!!
                 abletonbinType = tmp $ extToAbletonData $ takeExtensions path,
                 abletonbinData = dat
             }
    where
      tmp d = case d of 
          Just t -> t 
          _      -> undefined

writeAbletonFileBin :: (AbletonFile AbletonBin) -> IO FilePath
writeAbletonFileBin file = do
    B.writeFile (abletonfilePath file) $ abletonbinData $ abletonfileData file
    return $ abletonfilePath file






