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
module Command.Pull
  (
    pull,
    PullArgs (..)
  ) where

import App
import Files

--------------------------------------------------------------------------------
--  

-- | how to use the 'pull' command
data PullArgs = PullArgs
    {
    }


--------------------------------------------------------------------------------
--  the 'pull' command

pull :: PullArgs -> RIO' ()
pull args = do
    logInfo "pull not implemented yet."
