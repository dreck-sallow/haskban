{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Models where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import GHC.Generics

data Project = Project
  { projectId :: Int,
    projectName :: String,
    projectGroups :: [Group]
  }
  deriving (Show, Generic)

instance ToJSON Project

instance FromJSON Project

-- START -> Define project methods

getProjectId :: Project -> Int
getProjectId Project {..} = projectId

defaultProject :: Project
defaultProject = Project {projectId = 1, projectName = "2", projectGroups = []}

appendGroup :: Project -> Group -> Project
appendGroup project group =
  project {projectGroups = projectGroups project <> [group]}

tasksFromGroup :: Project -> Int -> [Task]
tasksFromGroup project' idx = groupTasks $ projectGroups project' !! idx

-- End <- define project methods

data Group = Group
  { -- groupId :: Int,
    groupName :: String,
    groupTasks :: [Task]
  }
  deriving (Show, Generic)

instance ToJSON Group

instance FromJSON Group

data Task = Task
  { taskId :: Int,
    taskTitle :: String,
    taskDesc :: String, -- Use Text instead of string? :?
    taskEndDate :: Maybe UTCTime
  }
  deriving (Show, Generic)

instance ToJSON Task

instance FromJSON Task
