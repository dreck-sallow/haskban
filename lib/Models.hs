{-# LANGUAGE DeriveGeneric #-}

module Models where

import Data.Time (UTCTime)
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

data Project = Project 
  {
    -- projectId :: Int,
    projectName :: String,
    projectGroups :: [Group]
  } deriving (Show, Generic)

instance ToJSON Project
instance FromJSON Project

data Group = Group 
  {
    -- groupId :: Int,
    groupName :: String,
    groupTasks :: [Task]
  } deriving (Show, Generic)


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
