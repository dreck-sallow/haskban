module Project where

import Directory (storageFolderPath)
-- import qualified Models as M
import qualified Model.Project as MP
import Storage (loadData, saveData)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)

loadProject :: String -> IO (Maybe MP.Project)
loadProject projectId = do
  path' <- filePath' projectId
  catchIOError (loadData path') (const $ return Nothing)
  where
    filePath' :: String -> IO FilePath
    filePath' id' = (</> (id' <> ".json")) <$> storageFolderPath

saveProject :: MP.Project -> IO ()
saveProject project = do
  storageFolder <- storageFolderPath
  saveData (storageFolder </> (show (MP.projectId project) ++ ".json")) project
