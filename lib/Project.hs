module Project where

import Directory (storageFolderPath)
import qualified Models as M
import Storage (loadData, saveData)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)

loadProject :: String -> IO (Maybe M.Project)
loadProject projectId = catchIOError (loadData (projectId <> ".jon")) (const $ return Nothing)

saveProject :: M.Project -> IO ()
saveProject project = do
  storageFolder <- storageFolderPath
  saveData (storageFolder </> (show (M.getProjectId project) ++ ".json")) project
