module Main where

import Ableton.Files 
import Ableton.XML


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

