module Directory (storageFolderName, storageFolderPath, storageFilePath) where

import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath (takeDirectory, (</>))

storageFolderName :: String
storageFolderName = "haskban"

storageFolderPath :: IO FilePath
storageFolderPath = getXdgDirectory XdgData storageFolderName

-- | Get the store mapping from /haskban/storage.json
storageFilePath :: IO FilePath
storageFilePath = do
  filePath' <- (</> "storage.json") <$> storageFolderPath
  existFilePath <- doesFileExist filePath'
  if existFilePath then return filePath' else createDirectoryIfMissing True (takeDirectory filePath') >> writeFile filePath' "" >> return filePath'
