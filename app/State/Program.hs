module State.Program where

import Data.Maybe (fromMaybe)
import qualified Models as M
import State.FocusIndex
import Utils (insertByIndex, removeByIndex, replaceByIndex, swapIndices)

type MoveIndex a = Index -> [a] -> Index

data ProgramState = ProgramState {project :: M.Project, focusCursor :: Cursor, selected :: Cursor}

-- work with focus cursor

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

-- work with select cursor

moveBackTask :: Cursor -> [M.Task] -> (Cursor, [M.Task])
moveBackTask (TaskI groupI taskI) tasks' = case prevListIndex (Just taskI) tasks' of
  Nothing -> (TaskI groupI taskI, tasks')
  Just i -> (TaskI groupI i, swapIndices i taskI tasks')
moveBackTask cursor tasks' = (cursor, tasks')

moveForwardTask :: Cursor -> [M.Task] -> (Cursor, [M.Task])
moveForwardTask (TaskI groupI taskI) tasks' = case nextListIndex (Just taskI) tasks' of
  Nothing -> (TaskI groupI taskI, tasks')
  Just i -> (TaskI groupI i, swapIndices i taskI tasks')
moveForwardTask cursor tasks' = (cursor, tasks')

moveBackGroup :: Cursor -> [M.Group] -> (Cursor, [M.Group])
moveBackGroup cursor groups' =
  case cursor of
    Empty -> (cursor, groups')
    (GroupI groupIdx) -> (GroupI (moveBack' groupIdx), swapBack' groupIdx)
    (TaskI groupIdx taskIdx) ->
      if groupIdx == 0
        then (cursor, groups')
        else
          let (currentTask', updatedCurrentGruop') = removeTaskInGroup' taskIdx (groups' !! groupIdx)
              prevGroupIdx' = moveBack' groupIdx
              (boundTaskIdx, updatedBackGroup') = insertTaskInGroup' taskIdx currentTask' (groups' !! prevGroupIdx')
           in (TaskI prevGroupIdx' boundTaskIdx, replaceByIndex groupIdx updatedCurrentGruop' $ replaceByIndex prevGroupIdx' updatedBackGroup' groups')
  where
    swapBack' :: Int -> [M.Group]
    swapBack' idx = swapIndices idx (moveBack' idx) groups'

    moveBack' :: Int -> Int
    moveBack' idx = fromMaybe 0 (prevListIndex (Just idx) groups')

    removeTaskInGroup' :: Int -> M.Group -> (M.Task, M.Group)
    removeTaskInGroup' idx group' =
      let currentTask' = M.groupTasks group' !! idx
       in (currentTask', group' {M.groupTasks = removeByIndex idx (M.groupTasks group')})

    insertTaskInGroup' :: Int -> M.Task -> M.Group -> (Int, M.Group)
    insertTaskInGroup' idx task group' =
      let boundIdx = fromMaybe 0 $ boundLinstIndex (Just idx) (M.groupTasks group')
       in (boundIdx, group' {M.groupTasks = insertByIndex boundIdx task (M.groupTasks group')})

moveForwardGroup :: Cursor -> [M.Group] -> (Cursor, [M.Group])
moveForwardGroup cursor groups' =
  case cursor of
    Empty -> (cursor, groups')
    (GroupI groupIdx) -> (GroupI (moveForward groupIdx), swapBack' groupIdx)
    (TaskI groupIdx taskIdx) ->
      if groupIdx == (length groups' - 1)
        then (cursor, groups')
        else
          let (currentTask', updatedCurrentGruop') = removeTaskInGroup' taskIdx (groups' !! groupIdx)
              prevGroupIdx' = moveForward groupIdx
              (boundTaskIdx, updatedBackGroup') = insertTaskInGroup' taskIdx currentTask' (groups' !! prevGroupIdx')
           in (TaskI prevGroupIdx' boundTaskIdx, replaceByIndex groupIdx updatedCurrentGruop' $ replaceByIndex prevGroupIdx' updatedBackGroup' groups')
  where
    swapBack' :: Int -> [M.Group]
    swapBack' idx = swapIndices idx (moveForward idx) groups'

    moveForward :: Int -> Int
    moveForward idx = fromMaybe 0 (nextListIndex (Just idx) groups')

    removeTaskInGroup' :: Int -> M.Group -> (M.Task, M.Group)
    removeTaskInGroup' idx group' =
      let currentTask' = M.groupTasks group' !! idx
       in (currentTask', group' {M.groupTasks = removeByIndex idx (M.groupTasks group')})

    insertTaskInGroup' :: Int -> M.Task -> M.Group -> (Int, M.Group)
    insertTaskInGroup' idx task group' =
      let boundIdx = fromMaybe 0 $ boundLinstIndex (Just idx) (M.groupTasks group')
       in (boundIdx, group' {M.groupTasks = insertByIndex boundIdx task (M.groupTasks group')})
