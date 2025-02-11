{-# LANGUAGE DeriveGeneric #-}

module Model.Group where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Model.Task (Task)
import Utils (insertByIndex, removeByIndex, replaceItm, swapIndices)

data Group = Group
  { groupName :: String,
    groupTasks :: [Task]
  }
  deriving (Generic)

instance FromJSON Group

instance ToJSON Group

newGroup :: String -> Group
newGroup name = Group {groupName = name, groupTasks = []}

withTasks :: [Task] -> Group -> Group
withTasks tasks group = group {groupTasks = tasks}

getTask :: Int -> Group -> Maybe Task
getTask idx group
  | idx < 0 || idx >= length (groupTasks group) = Nothing
  | otherwise = Just $ groupTasks group !! idx

modifyTask :: Int -> (Task -> Task) -> Group -> Group
modifyTask idx fn group = case getTask idx group of
  Nothing -> group
  Just tsk -> group {groupTasks = replaceItm idx (fn tsk) (groupTasks group)}

swapTaskByIndex :: (Int, Int) -> Group -> Group
swapTaskByIndex (x, y) group = group {groupTasks = swapIndices x y (groupTasks group)}

deleteTaskByIndex :: Int -> Group -> Group
deleteTaskByIndex idx group = group {groupTasks = removeByIndex idx (groupTasks group)}

insertTaskByIndex :: Int -> Task -> Group -> Group
insertTaskByIndex idx task group = group {groupTasks = insertByIndex idx task (groupTasks group)}
