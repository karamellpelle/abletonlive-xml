module Find
  (
  ) where

import MyPrelude
--------------------------------------------------------------------------------
--  find Ableton files

{-

-- | find Ableton XML files based on a list of files and folders
findFilePathsAbletonXML :: [FilePath] -> IO [FilePath]
findFilePathsAbletonXML = findFilePaths filepathIsAbletonXML

findFilePathsAbletonBin :: [FilePath] -> IO [FilePath]
findFilePathsAbletonBin = findFilePaths filepathIsAbletonBin

findFilePaths :: (FilePath -> Bool) -> [FilePath] -> IO [FilePath]
findFilePaths pred paths =
    fmap concat
    case path of 
        folder -> mapM findFilePathsXML (++) [] foldercontent
        file  -> [file]
            case extension of 
                ".xml"  -> path
                _ -> []
        _ -> return [] -- should not happen

readAbletonXMLs:: [FilePath] -> IO (AbletonFile AbletonXML)
readAbletonXMLs = findFilePathsXML >>= readAbletonFileXMLS


findFilePathsXML :: [] 
    files <- case fileargs of 
        [] -> findAbletonFileBin
class ToAbletonXML a where
    toAbletonXML :: a -> Maybe AbletonXML 
-}

--copyToAbletonFile :: FilePath -> IO FilePath
--copyToAbletonFile path dir = do
--    undefined
    --when (isAbletonFile path) $ 
    --when (isAbletonXML path) $ 
    -- read file type

{-
writeAbletonXML =<< (fmap toAbletonXML) readAbletonXML


    (fmap toAbletonFile $ readAbletonXML path) >>= writeAbletonFile
    
    (a -> b) -> (a -> m a) -> a -> m b
readAbletonXML > toAbletonFile > writeAbletonFile

--changeRoot "/a/share" "/b/myableton" $ toAbletonFile axml
-}


