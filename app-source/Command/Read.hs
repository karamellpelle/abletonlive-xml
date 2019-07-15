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

import App
import Files
import Ableton

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
      
    --ps <- findPathsWith (\p -> pure True) $ if null paths then ["."] else paths
    -- find .xml files. if no filepaths given, use current directory
    ps <- findPathsWith (\p -> pure $ filepathIsAbletonXML p) $ if null paths then ["."] else paths
    mapM_ putStrLn ps

    where
      putStrLn str = 
          logInfo $ fromString str
      readFiles path = do
          readAbletonFileXML path >>= \lr -> case lr of
              Left err    -> logWarn $ display err
              Right file  -> logInfo $ "successfully read '" <> fromString path <> "'"
