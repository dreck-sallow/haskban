module Directory (storageFolderName, storageFolderPath, storageFilePath) where

import System.Directory (XdgDirectory (XdgData), getXdgDirectory)
import System.FilePath ((</>))

storageFolderName :: String
storageFolderName = "haskban"

storageFolderPath :: IO FilePath
storageFolderPath = getXdgDirectory XdgData storageFolderName

storageFilePath :: IO FilePath
storageFilePath = do
  folderPath <- storageFolderPath
  return $ folderPath </> "storage.json"
