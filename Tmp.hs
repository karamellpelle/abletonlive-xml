import Ableton
import Ableton.Convert
import Ableton.AbletonFile
import System.EasyFile

maybeMapFile :: (a -> Maybe b) -> AbletonFile a -> Maybe (AbletonFile b)
maybeMapFile f (AbletonFile path a) = 
    case f a of
        Just b  -> Just $ AbletonFile path b
        Nothing -> Nothing

--------------------------------------------------------------------------------
--  

createXMLFile :: FilePath -> IO FilePath
createXMLFile path = do
    file <- readAbletonFileBin path
    case toAbletonXML file of
        Nothing   -> return ""
        Just content  -> writeAbletonFileXML $ AbletonFile (changeExt path) content
    where
      changeExt = \path -> addExtension path ".xml"

