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
module Command.Read
  (
    read,
    ReadArgs (..)
  ) where

import RIO
import App
import Files

--------------------------------------------------------------------------------
--  

-- | how to use the 'read' command
data ReadArgs = ReadArgs
    {
        readargsFilePaths :: [FilePath]
    }


--------------------------------------------------------------------------------
--  the 'read' command

read :: ReadArgs -> RIO' ()
read args = do
    let paths = readargsFilePaths args
    logInfo "files to read:"
    mapM_ logInfo $ map fromString $ readargsFilePaths args
    logInfo ""
    mapM_ readFiles paths
    where
      readFiles path = do
          readAbletonFileXML path >>= \either -> case either of
              Left err    -> logWarn $ display err
              Right file  -> logInfo $ "successfully read '" <> fromString path <> "'"
