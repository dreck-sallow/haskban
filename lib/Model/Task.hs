{-# LANGUAGE DeriveGeneric #-}

module Model.Task where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Task = Task
  { taskTitle :: String,
    taskId :: Int,
    taskContent :: String
  }
  deriving (Generic)

instance FromJSON Task

instance ToJSON Task

newTask :: Int -> String -> String -> Task
newTask id' title content = Task {taskId = id', taskTitle = title, taskContent = content}
