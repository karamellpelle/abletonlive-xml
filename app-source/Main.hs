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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}
module Main where

import Options.Applicative.Simple
import Development.GitRev
import Paths_abletonlive_xml

import App
import Command

import RIO.Process


-- | TODO: find this from stack package (Paths_abletonlive_xml does not expose it)
executableName :: String
executableName = "abletonlive-xml"


--------------------------------------------------------------------------------
--  magic starts here


main :: IO ()
main = do

  -- what are we actually doing??
  (opts, cmd) <- getGlobalOptsCommands

  --verbose <- isJust <$> lookupEnv "RIO_VERBOSE"
  lo <- logOptionsHandle stderr False --verbose
  po <- mkDefaultProcessContext

  withLogFunc lo $ \lf ->
    let run = Runner
              { runnerLogFunc = lf
              , runnerProcessContext = po
              , runnerGlobalOpts = opts
              }
     in runRIO run $ rioApplication cmd
 

-- | RIO starts here!
rioApplication :: Command -> RIO' ()
rioApplication cmd = do
    logInfo $ fromString $ "we are running!" 
    case cmd of
      CommandRead args  -> read args
      CommandWrite args -> write args
      CommandPush args  -> push args
      CommandPull args  -> pull args
      _                 -> pure ()


--------------------------------------------------------------------------------
--  

-- | retrieve commands and arguments from command line
getGlobalOptsCommands :: IO (GlobalOpts, Command)
getGlobalOptsCommands = do
    simpleOptions versionStr headerStr descriptionStr 
                  pGlobalOpts pCommand
    where
      versionStr = $(simpleVersion Paths_abletonlive_xml.version)
      headerStr = executableName ++ " : work with XML files instead of Ableton Live binary files"
      descriptionStr = "for command specific help, use '" ++ executableName ++ " COMMAND --help'"

      pGlobalOpts = do
          -- verbose output? (optional)
          verbose <- switch (long "verbose" <> short 'v' <> help "verbose output")

          pure $ GlobalOpts { globaloptsVerbose = verbose }
          
      pCommand = do
          addCommand "read" "read XML files from binary files" id $ do
              -- files/folders (optional). default all
              filepaths <- many $ strArgument (metavar "PATHS" <> help "file/folders to read (optional)")

              pure $ CommandRead $ ReadArgs filepaths 
              
          addCommand "write" "write binary files from XML files" id $ do
              -- files/folders (optional). default all
              filepaths <- many $ strArgument (metavar "PATHS" <> help "file/folders to write (optional)")

              pure $ CommandWrite $ WriteArgs filepaths

          addCommand "push" "push changes to remote Git repository" id $ do
              -- using specific git repository (optional)
              optGitRepository <- strOption (long "git-repository" <> metavar "URL|REMOTE" <> help "Git repository")
                                  <|> pure ""
              pure $ CommandPush $ PushArgs optGitRepository

          addCommand "pull" "pull changes from remote Git repository" id $ do
              let args = PullArgs
              pure $ CommandPull args









