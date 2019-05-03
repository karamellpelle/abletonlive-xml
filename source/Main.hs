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
module Main where

import Ableton.AbletonFile
import Ableton.AbletonXML


main :: IO ()
main = 
  printHelp


printHelp :: IO ()
printHelp = do
    putStrLn "abletonlive-xml - let Ableton Live use XML & Git"
    putStrLn ""
    putStrLn "Usage: abletonlive-xml [--help]"
    putStrLn "                       [--version]"
    putStrLn "                       [--verbose]"
    putStrLn "                       [--silent]"
    putStrLn "                       [--read-dir XML-DIR]"
    putStrLn "                       [--write-dir ABLETON-DIR]"
    putStrLn "                       [read|write|pull|push]"
    putStrLn ""
    putStrLn "Available options:"
    putStrLn "  --help                     Show this help text"
    putStrLn "  --version                  Show version"
    putStrLn "  --verbose                  Enable verbose mode"
    putStrLn "  --silent                   Enable silent mode"
    putStrLn "  --xml-dir XML-DIR          Use XML-DIR for input (default './xml')"
    putStrLn "  --ableton-dir ABLETON-DIR  Use ABLETON-DIR for output (default: './')"
    putStrLn ""
    putStrLn "Available commands:"
    putStrLn "  read [ABLETON-FILES|ABLETON-DIRS]       (default all)"
    putStrLn "  write [ABLETON-FILES|ABLETON-DIRS]      (default all)"
    putStrLn "  pull                     Pull changes in Git repository"
    putStrLn "  push                     Push changes to Git repository"

printVersion :: IO ()
printVersion = do
    putStrLn "1.9.3 x86_64 (FIXME!!!)"

