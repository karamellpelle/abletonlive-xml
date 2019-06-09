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
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}



module Main where

import RIO
import RIO.Process
import App
import Paths_abletonlive_xml
import Options.Applicative.Simple

import Ableton
import Ableton.AbletonBin
import Ableton.AbletonXML


main :: IO ()
main = do
  --verbose <- isJust <$> lookupEnv "RIO_VERBOSE"
  lo <- logOptionsHandle stderr False --verbose
  po <- mkDefaultProcessContext
  opts <- getGlobalOpts

  withLogFunc lo $ \lf ->
    let run = Runner
              { runnerLogFunc = lf
              , runnerProcessContext = po
              , runnerGlobalOpts = opts
              }
     in runRIO run runApplication
  



getGlobalOpts :: IO GlobalOpts
getGlobalOpts = do
    (options, ()) <- simpleOptions
        -- $(simpleVersion Paths_abletonlive_xml.version)
        "0.0"
        "Header for command line arguments"
        "Program description, also for command line arguments"
        (GlobalOpts
           <$> switch ( long "verbose"
                     <> short 'v'
                     <> help "Verbose output?"
                      )
        )
        empty

    return options

-- | RIO starts here!
runApplication :: RIO Runner ()
runApplication = do
    logInfo "we are running!"
    

-- | temporary helper
printHelp :: HasLogFunc env => RIO env ()
printHelp = do
    logInfo "abletonlive-xml - let Ableton Live use XML & Git"
    logInfo ""
    logInfo "Usage: abletonlive-xml [--help]"
    logInfo "                       [--version]"
    logInfo "                       [--verbose]"
    logInfo "                       [--silent]"
    logInfo "                       [--read-dir XML-DIR]"
    logInfo "                       [--write-dir ABLETON-DIR]"
    logInfo "                       [read|write|pull|push]"
    logInfo ""
    logInfo "Available options:"
    logInfo "  --help                     Show this help text"
    logInfo "  --version                  Show version"
    logInfo "  --verbose                  Enable verbose mode"
    logInfo "  --silent                   Enable silent mode"
    logInfo "  --xml-dir XML-DIR          Use XML-DIR for input (default './xml')"
    logInfo "  --ableton-dir ABLETON-DIR  Use ABLETON-DIR for output (default: './')"
    logInfo ""
    logInfo "Available commands:"
    logInfo "  read [ABLETON-FILES|ABLETON-DIRS]       (default all)"
    logInfo "  write [ABLETON-FILES|ABLETON-DIRS]      (default all)"
    logInfo "  pull                     Pull changes in Git repository"
    logInfo "  push                     Push changes to Git repository"


printVersion :: HasLogFunc env => RIO env () 
printVersion = do
    logInfo "1.9.3 x86_64 (FIXME!!!)"

