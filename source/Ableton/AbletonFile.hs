module Ableton.AbletonFile
  (
    AbletonFile (..),
  ) where

import Ableton.AbletonFileType
import MyPrelude


data AbletonFile = 
    AbletonFile
    {
        abletonfilePath :: FilePath,
        abletonfileType :: AbletonFileType,
        abletonfileContent :: ByteString
    }


