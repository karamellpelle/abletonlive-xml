module Ableton
  (
    readAbletonXML,
    writeAbletonXML,
    readAbletonFile,
    writeAbletonFile
  ) where

--------------------------------------------------------------------------------
--  read & write AbletonXML


-- | no verification of file content; it assumes correct XML definition
readAbletonXML :: FilePath -> IO AbletonXML
readAbletonXML path = do
    content <- BS.readFile path
    return $ AbletonXML
             {
                abletonxmlPath = path,
                abletonxmlContent = content
             }
    

writeAbletonXML :: AbletonXML -> IO FilePath
writeAbletonXML axml =
    BS.writeFile (abletonxmlPath axml) $ abletonxmlContent axml
    return $ abletonxmlPath axml


--------------------------------------------------------------------------------
--  read & write AbletonFile

-- | no verification of file content; it assumes correct extension
readAbletonFile :: FilePath -> IO AbletonFile
readAbletonFile path = do
    content <- BS.readFile path
    return $  AbletonXML
              {
                abletonfilePath = path,
                abletonfileType = fromJust $ extensionToAbletonFileType $ takeExtensions path,
                abletonfileContent = content
              }

    fmap toAbletonXML $ BS.readFile path

writeAbletonFile :: AbletonFile -> IO FilePath
writeAbletonFile afile = do
    BS.writeFile (abletonfilePath afile) $ abletonfileContent afile
    return $ abletonfilePath afile
    


copyToAbletonFile :: FilePath -> IO FilePath
copyToAbletonFile path dir = do
    when (isAbletonFile path) $ 
    when (isAbletonXML path) $ 
    -- read file type

writeAbletonXML =<< (fmap toAbletonXML) readAbletonXML


    (fmap toAbletonFile $ readAbletonXML path) >>= writeAbletonFile
    
    (a -> b) -> (a -> m a) -> a -> m b
readAbletonXML > toAbletonFile > writeAbletonFile

--changeRoot "/a/share" "/b/myableton" $ toAbletonFile axml
