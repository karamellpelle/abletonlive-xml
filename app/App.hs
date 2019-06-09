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
module App
  (
    Runner (..)
  , GlobalOpts (..)
  , HasGlobalOpts (..)
  ) where

import RIO
import RIO.Process

-- | inspired by Stack.Types.Config of commercialhaskell/stack on GitHub,
--   and RIO.Prelude.Simple.SimpleApp
data Runner = Runner
    { runnerLogFunc :: !LogFunc
    , runnerProcessContext :: !ProcessContext
    , runnerGlobalOpts :: !GlobalOpts
    }

data GlobalOpts = GlobalOpts
    { globaloptsVerbose :: !Bool
    }


--------------------------------------------------------------------------------
--  HasGlobalOpts

class HasGlobalOpts env where
    globalOptsL :: Lens' env GlobalOpts

instance HasGlobalOpts Runner where
    globalOptsL = lens runnerGlobalOpts (\x y -> x { runnerGlobalOpts = y })

--------------------------------------------------------------------------------
--  HasLogFunc

instance HasLogFunc Runner where
    logFuncL = lens runnerLogFunc (\x y -> x { runnerLogFunc = y })

--------------------------------------------------------------------------------
--  HasProcessContext

instance HasProcessContext Runner where
    processContextL = lens runnerProcessContext (\x y -> x { runnerProcessContext = y })


    
