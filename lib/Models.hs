{-# LANGUAGE DeriveGeneric #-}

module Models where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import GHC.Generics (Generic)
import Utils (replaceByIndex)

data Project = Project
  { projectId :: Int,
    projectName :: String,
    projectGroups :: [Group]
  }
  deriving (Generic)

tasksFromGroup :: Project -> Int -> [Task]
tasksFromGroup project' idx
  | idx < 0 || idx >= length (projectGroups project') = []
  | otherwise = groupTasks $ projectGroups project' !! idx

updateProjectGroup :: Int -> Project -> (Group -> Group) -> Project
updateProjectGroup idx project f
  | idx < 0 || idx > (length groups' - 1) = project
  | otherwise = project {projectGroups = updatedProjectGroups}
  where
    groups' :: [Group]
    groups' = projectGroups project

    updatedProjectGroups :: [Group]
    updatedProjectGroups = replaceByIndex idx (f (groups' !! idx)) groups'

instance ToJSON Project

instance FromJSON Project

data Group = Group
  { groupName :: String,
    groupTasks :: [Task]
  }
  deriving (Generic)

updateGroupTask :: Int -> Group -> (Task -> Task) -> Group
updateGroupTask idx group f
  | idx < 0 || idx > (length tasks' - 1) = group
  | otherwise = group {groupTasks = updatedGroupTasks}
  where
    updatedGroupTasks :: [Task]
    updatedGroupTasks = replaceByIndex idx (f (tasks' !! idx)) tasks'

    tasks' :: [Task]
    tasks' = groupTasks group

instance ToJSON Group

instance FromJSON Group

data Task = Task
  { taskId :: Int,
    taskTitle :: String,
    taskContent :: String
    -- taskEndDate :: Maybe UTCTime
  }
  deriving (Generic)

instance ToJSON Task

instance FromJSON Task
