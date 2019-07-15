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
    
    findPathsWith,

  ) where

import RIO.Directory
import RIO.FilePath
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Char as C
import qualified RIO.Text as T
import qualified RIO.List as P -- tmp

import Ableton
import App



--------------------------------------------------------------------------------
--  file names

--findPathsWith :: MonadUnliftIO m => (a -> m Bool) -> [FilePath] -> m [FilePath]
-- | find files and directories based on a list of files and directories.
--   exceptions from 'pred' will not be handled, and 'pred' should therefore probly handle exceptions itself
--   TODO: add exception handler argument 
findPathsWith :: (FilePath -> RIO' Bool) -> [FilePath] -> RIO' [FilePath]
findPathsWith pred = helper ""
    where 
      helper dir = \ps -> case ps of 
          []        -> pure []
          (p:ps')    -> do
              let path = dir </> p
              --logInfo $ fromString $ "(p:ps') -> " ++ path
              pathType path >>= \pt -> case pt of
                  IsDir     -> do
                      --logInfo $ fromString $ "IsDir: " ++ path
                      ps'' <- listDirectory path `catchIO` \err -> do
                              logWarn $ fromString $ "Could not read directory: " ++ show err
                              pure []
                      --logInfo $ fromString $ "ListDir: " ++ P.concat (P.intersperse " " ps'' )
                      helper path ps'' >>= \qs -> fmap (qs ++) $ helper dir ps'
                  IsFile    -> do
                      --logInfo $ fromString $ "IsFile: " ++ path
                      pred path >>= \ok -> case ok of -- ^ 'pred' may throw exceptions here, but should not if the programmer reads documentation and does right
                          False -> helper dir ps'
                          True  -> fmap (path:) $ helper dir ps'
                  IsNone    -> do
                      logWarn $ fromString $ "Filepath does not exist: " ++ path
                      helper dir ps'


-- | FIXME: does doesFile/DirectoryExist throw IOException?
data PathType = IsFile | IsDir | IsNone
pathType :: MonadUnliftIO m => FilePath -> m PathType
pathType p = do
    isfile <- doesFileExist p
    isdir  <- doesDirectoryExist p
    let pt | isfile  = IsFile
           | isdir   = IsDir
           | otherwise = IsNone
    pure pt


--readAbletonFilesXML :: [FilePath] -> 
