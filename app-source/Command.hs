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
module Command
  (
      Command (..),
      module Command.Read,
      module Command.Write,
      module Command.Pull,
      module Command.Push
  ) where


import App
import Command.Read
import Command.Write
import Command.Push
import Command.Pull

--------------------------------------------------------------------------------
--  


-- | the available commands
data Command = 
    CommandRead  ReadArgs  |
    CommandWrite WriteArgs |
    CommandPush  PushArgs  |
    CommandPull  PullArgs


    
