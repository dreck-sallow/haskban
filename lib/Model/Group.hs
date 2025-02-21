{-# LANGUAGE DeriveGeneric #-}

module Model.Group where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Model.Task (Task)
import Util.List (insertByIndex, removeByIndex, replaceByIndex, swapIndices)

data Group = Group
  { groupName :: String,
    groupTasks :: [Task]
  }
  deriving (Generic)

instance FromJSON Group

instance ToJSON Group

newGroup :: String -> Group
newGroup name = Group {groupName = name, groupTasks = []}

hasTasks :: Group -> Bool
hasTasks group = not $ null (groupTasks group)

withTasks :: [Task] -> Group -> Group
withTasks tasks group = group {groupTasks = tasks}

setTasks :: ([Task] -> [Task]) -> Group -> Group
setTasks fn group = group {groupTasks = fn (groupTasks group)}

getTask :: Int -> Group -> Maybe Task
getTask idx group
  | idx < 0 || idx >= length (groupTasks group) = Nothing
  | otherwise = Just $ groupTasks group !! idx

modifyTask :: Int -> (Task -> Task) -> Group -> Group
modifyTask idx fn group = case getTask idx group of
  Nothing -> group
  Just tsk -> group {groupTasks = replaceByIndex idx (fn tsk) (groupTasks group)}

swapTaskByIndex :: (Int, Int) -> Group -> Group
swapTaskByIndex (x, y) group = group {groupTasks = swapIndices (x,y) (groupTasks group)}

deleteTaskByIndex :: Int -> Group -> Group
deleteTaskByIndex idx group = group {groupTasks = removeByIndex idx (groupTasks group)}

insertTaskByIndex :: Int -> Task -> Group -> Group
insertTaskByIndex idx task group = group {groupTasks = insertByIndex idx task (groupTasks group)}
