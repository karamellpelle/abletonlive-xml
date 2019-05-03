module Ableton.AbletonXML
  (
    AbletonXML (..),
  ) where



data AbletonXML = 
    AbletonXML
    {
        abletonxmlPath :: FilePath,
        abletonxmlContent :: ByteString
    }

