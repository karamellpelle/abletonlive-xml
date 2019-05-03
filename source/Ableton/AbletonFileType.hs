module Ableton.AbletonFileType
  (
    AbletonFileType (..),
    extensionToAbeltonFileType,
    abletonfiletypeToExtension
  ) where

-- | https://help.ableton.com/hc/en-us/articles/209769625-Live-specific-file-types-adg-als-alp-
data AbletonFileType = 
    FileADG | -- ^ device group
    FileAGR | -- ^ groove file
    FileADV | -- ^ device preset
    FileALC | -- ^ Live clip
    FileALS | -- ^ Live set
    FileALP | -- ^ Live pack
    FileAMS | -- ^ meta sound
    FileAMXD | -- ^ max for Live
    FileASD | -- ^ warp analysis
    FileASX   -- ^ skin file
    

--------------------------------------------------------------------------------
--  

extensionToAbeltonFileType :: String -> Maybe AbletonFileType
extensionToAbeltonFileType ext = case fmap toLower ext of -- make sure case does not matter
      ".adg" -> Just FileADG 
      ".agr" -> Just FileAGR
      ".adv" -> Just FileADV
      ".alc" -> Just FileALC
      ".als" -> Just FileALS
      ".alp" -> Just FileALP
      ".ams" -> Just FileAMS
      ".amxd" -> Just FileAMXD
      ".asd" -> Just FileASD
      ".asx" -> Just FileASX
      _     -> Nothing

abletonfiletypeToExtension :: AbletonFileType -> String 
abletonfiletypeToExtension t = case t of 
    FileADG -> ".adg"
    FileAGR -> ".agr"
    FileADV -> ".adv"
    FileALC -> ".alc"
    FileALS -> ".als"
    FileALP -> ".alp"
    FileAMS -> ".ams"
    FileAMXD -> ".amxd"
    FileASD -> ".asd"
    FileASX -> ".asx"
    
