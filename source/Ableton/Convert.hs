module Ableton.Convert
  (
    toAbletonFile,
    toAbletonXML,
  ) where

import Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import System.EasyFile

import Ableton.AbletonFileType

--------------------------------------------------------------------------------
--  modify path

class ModifyPath a where
    modifyPath :: (FilePath -> FilePath) -> a -> a
    -- ^ 

instance ModifyPath AbletonFile where
    modifyPath f afile = 
        afile { abletonfilePath = f $ abletonfilePath afile }

instance ModifyPath AbletonXML where
    modifyPath f axml = 
        axml { abletonxmlPath = f $ abletonxmlPath axml }

instance ModifyPath FilePath where
    modifyPath f = f



-- | change root folder, for example
--
--      changeRoot "xml/" "../" "xml/Packs/HiTech/Bass/CoolB.adg.xml" == "../Packs/HiTech/Bass/CoolB.adg.xml"
--
--      changeRoot "" "xml/" "Packs/HiTech/Bass/CoolB.adg" == "xml/Packs/HiTech/Bass/CoolB.adg"
--
-- | TODO: work with normalized paths. 
--         and think thourg what happens if a path is absolute, cf. semantics of '</>'
changeRoot :: ModifyPath a => FilePath -> FilePath -> a -> a
changeRoot root root' = modifyPath helper
    where
      helper = \path -> 
          case stripPrefix root path of
              Just path' -> root' </> path'
              Nothing    -> path




--------------------------------------------------------------------------------
--  ToAbletonFile

-- | convert a type (typically file data) to an `AbletonFile`
--   TODO: Maybe
class ToAbletonFile a where
    toAbletonFile :: a -> AbletonFile



-- | `AbletonFile -> AbletonFile`
instance ToAbletonFile AbletonFile where
    toAbletonfFile = id

-- | `AbletonXML -> AbletonFile`
instance ToAbletonFile AbletonXML where
    toAbletonfFile axml = 
        AbletonFile
        { 
            abletonfilePath = changePath $ abletonxmlPath axml,
            abletonfileType = filetype,
            abletonfileContent = GZip.compress $ abletonxmlContent axml -- use compressWith for control over compression parameters
        }
        where
          filetype = fromJust $ abletonxmlType axml -- assuming valid XML, no verificationD of valid XML
          changePath path = 
              let (path', ext) = splitExtensions path
              in  path' <.> abletonfiletypeToExtension filetype




--------------------------------------------------------------------------------
--  ToAbletonXML

-- | convert a type (typically file data) to an `AbletonXML`
class ToAbletonXML a where
    toAbletonXML :: a -> AbletonXML


-- | `AbletonFile -> AbletonXML`
instance ToAbletonXML AbletonXML where
    toAbletonXML = id

-- | `AbletonFile -> AbletonXML`
instance ToAbletonXML AbletonFile where
    toAbletonXML afile = 
        AbletonXML
        {
            abletonxmlPath = changePath $ abletonfilePath afile
            abletonxmlContent =  GZip.decompress $ abletonfileContent afile,
        }
        where
          changePath path =
              addExtension path ".xml" -- just add .xml to current extension, i.e. file.adg -> file.adg.xml


--------------------------------------------------------------------------------
--  peek AbletonFileType from XML data

abletonxmlType :: AbletonXML -> Maybe AbletonFileType
abletonxmlType xml = 
    undefined
{-

    case abletonxmlType axml of
    uKk
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
        
    undefined
-}
-- ^ FIXME: look at XML definition
