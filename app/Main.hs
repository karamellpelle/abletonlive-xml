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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}



module Main where

import RIO
import RIO.Process
import App
import Paths_abletonlive_xml
import Options.Applicative.Simple
import Development.GitRev

import Ableton
import Ableton.AbletonBin
import Ableton.AbletonXML


main :: IO ()
main = do
  --verbose <- isJust <$> lookupEnv "RIO_VERBOSE"
  lo <- logOptionsHandle stderr False --verbose
  po <- mkDefaultProcessContext
  (opts, cmd) <- getGlobalOptsCommands

  withLogFunc lo $ \lf ->
    let run = Runner
              { runnerLogFunc = lf
              , runnerProcessContext = po
              , runnerGlobalOpts = opts
              }
     in runRIO run $ runApplication cmd
  


getGlobalOptsCommands :: IO (GlobalOpts, Command)
getGlobalOptsCommands = do
    simpleOptions versionStr headerStr descriptionStr 
                  pGlobalOpts pCommand
    where
      versionStr = $(simpleVersion Paths_abletonlive_xml.version) -- ++ "  build " ++ take 7 $(gitHash)  -- short hash
      headerStr = "abletonlive-xml : work with XML files instead of Ableton Live binary files"
      descriptionStr = ""

      pGlobalOpts = do
          verbose <- switch (long "verbose" <> short 'v' <> help "verbose output")
          pure $ GlobalOpts { globaloptsVerbose = verbose }
          
      pCommand = do
          addCommand "read" "read XML from binary" id $ do
              let args = ["file.xml"] -- tmp
              pure $ CommandRead args
              
          addCommand "write" "write binary from XML" id $ do
              let args = ["file.axx"]
              pure $ CommandWrite args

          addCommand "pull" "pull changes from remote Git repository" id $ do
              let args = ()
              pure $ CommandPush args

          addCommand "push" "push changes to remote Git repository" id $ do
              let args = ()
              pure $ CommandPull args



-- | RIO starts here!
runApplication :: Command -> RIO Runner ()
runApplication cmd = do
    logInfo $ fromString $ "we are running!" ++ " (" ++ show cmd ++ ")"
    



