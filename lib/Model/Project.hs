{-# LANGUAGE DeriveGeneric #-}

module Model.Project where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Models (Group)
import Utils (currentTimestamp, insertByIndex, removeByIndex, replaceItm, swapIndices)

data Project = Project
  { projectId :: Int,
    projectName :: String,
    projectGroups :: [Group]
  }
  deriving (Generic)

instance FromJSON Project

instance ToJSON Project

-- | Construct a Project with Name, and using the timestamp as Id
newProject :: String -> IO Project
newProject name = do
  id' <- currentTimestamp
  return Project {projectId = id', projectName = name, projectGroups = []}

getGroup :: Int -> Project -> Maybe Group
getGroup idx project
  | idx < 0 || idx >= length (projectGroups project) = Nothing
  | otherwise = Just $ projectGroups project !! idx

modifyGroup :: Int -> (Group -> Group) -> Project -> Project
modifyGroup idx fn project = case getGroup idx project of
  Nothing -> project
  Just tsk -> project {projectGroups = replaceItm idx (fn tsk) (projectGroups project)}

swapGroupByIndex :: (Int, Int) -> Project -> Project
swapGroupByIndex (x, y) project = project {projectGroups = swapIndices x y (projectGroups project)}

deleteGroupByIndex :: Int -> Project -> Project
deleteGroupByIndex idx project = project {projectGroups = removeByIndex idx (projectGroups project)}

insertGroupByIndex :: Int -> Group -> Project -> Project
insertGroupByIndex idx group project = project {projectGroups = insertByIndex idx group (projectGroups project)}
