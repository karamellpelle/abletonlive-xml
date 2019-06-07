import Ableton
import Ableton.Convert
import Ableton.AbletonFile
import System.EasyFile

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
{-
-- | no verification of file dat; it assumes correct extension
readAbletonFileBin :: FilePath -> IO (Maybe (AbletonFile AbletonBin))
readAbletonFileBin path = do
    handle exception $ do
        dat <- BS.readFile path
        return $ Just $ AbletonFile path $ AbletonBin
                 {
                     abletonbinType = fromJust $ extToAbletonData $ takeExtensions path, -- no verification done!
                     abletonbinData = dat
                 }
    where 
      exception :: IOError -> IO (Maybe (AbletonFile AbletonBin))
      exception e = return Nothing
-}
