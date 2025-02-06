module State.Program where

import Data.Maybe (fromMaybe)
import qualified Models as M
import State.FocusIndex

type MoveIndex a = Index -> [a] -> Index

data ProgramState = ProgramState {project :: M.Project, focusCursor :: Cursor}

nextTask :: Cursor -> M.Project -> Cursor
nextTask Empty _ = Empty
nextTask (GroupI groupI) project' = if not $ null (M.tasksFromGroup project' groupI) then TaskI groupI 0 else GroupI groupI
nextTask (TaskI groupI' taskI') project' = TaskI groupI' (fromMaybe 0 nextIndex)
  where
    nextIndex :: Index
    nextIndex = nextListIndex (Just taskI') (M.tasksFromGroup project' groupI')

prevTask :: Cursor -> M.Project -> Cursor
prevTask (TaskI groupI' taskI') project' = if taskI' == 0 then GroupI groupI' else TaskI groupI' (fromMaybe 0 prevIndex)
  where
    prevIndex :: Index
    prevIndex = prevListIndex (Just taskI') (M.tasksFromGroup project' groupI')
prevTask cursor _ = cursor

boundTaskIndex :: Int -> Index -> [M.Task] -> Cursor
boundTaskIndex groupI' maybeTaskI tasks = case boundLinstIndex maybeTaskI tasks of
  Nothing -> GroupI groupI'
  Just i -> TaskI groupI' i

transferTaskIdx :: Cursor -> M.Project -> Cursor
transferTaskIdx Empty _ = Empty
transferTaskIdx (GroupI groupI) project' = boundTaskIndex groupI Nothing (M.tasksFromGroup project' groupI)
transferTaskIdx (TaskI groupI taskI) project' = boundTaskIndex groupI (Just taskI) (M.tasksFromGroup project' groupI)

nextGroup :: Cursor -> M.Project -> Cursor
nextGroup Empty project' = if not $ null (M.projectGroups project') then GroupI 0 else Empty
nextGroup (GroupI groupI) project' =
  maybe
    Empty
    GroupI
    (nextListIndex (Just groupI) (M.projectGroups project'))
nextGroup (TaskI groupI taskI) project' = maybe Empty (\i -> transferTaskIdx (TaskI i taskI) project') nextGroupIndex
  where
    nextGroupIndex :: Index
    nextGroupIndex = nextListIndex (Just groupI) (M.projectGroups project')

prevGroup :: Cursor -> M.Project -> Cursor
prevGroup Empty _ = Empty
prevGroup (GroupI groupI) project' =
  maybe
    Empty
    GroupI
    (prevListIndex (Just groupI) (M.projectGroups project'))
prevGroup (TaskI groupI taskI) project' = maybe Empty (\i -> transferTaskIdx (TaskI i taskI) project') nextGroupIndex
  where
    nextGroupIndex :: Index
    nextGroupIndex = prevListIndex (Just groupI) (M.projectGroups project')
