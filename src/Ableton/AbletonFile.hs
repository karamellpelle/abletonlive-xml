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
module Ableton.AbletonFile
  (
    AbletonFile (..),

    -- TODO: remove
    changeAbletonFilePath,
    changeAbletonFileRoot,
    changeAbletonFileBaseName,

  ) where

import MyPrelude
import System.EasyFile

import Ableton.AbletonData



-- | use GADTs?
data AbletonFile a = 
    AbletonFile
    {
        abletonfilePath :: FilePath,
        abletonfileContent :: a
    }


--------------------------------------------------------------------------------
--  path operations

changeAbletonFilePath :: (FilePath -> FilePath) -> AbletonFile a -> AbletonFile a
changeAbletonFilePath f file =
    file { abletonfilePath = f $ abletonfilePath file }


-- | change root folder, for example
--
--      changeAbletonFileRoot "xml/" "../" "xml/Packs/HiTech/Bass/CoolB.adg.xml" == "../Packs/HiTech/Bass/CoolB.adg.xml"
--
--      changeAbletonFileRoot "" "xml/" "Packs/HiTech/Bass/CoolB.adg" == "xml/Packs/HiTech/Bass/CoolB.adg"
--
changeAbletonFileRoot :: FilePath -> FilePath -> AbletonFile a -> AbletonFile a
changeAbletonFileRoot root root' = 
    changeAbletonFilePath $ \path ->
        case stripPrefix root path of
            Just path' -> root' </> path'
            Nothing    -> path
        -- ^ TODO: work with normalized paths. 
        --         and think thourg what happens if a path is absolute, cf. semantics of '</>'

changeAbletonFileBaseName :: String -> AbletonFile a -> AbletonFile a
changeAbletonFileBaseName name = 
    changeAbletonFilePath $ \path -> replaceBaseName path name

    
