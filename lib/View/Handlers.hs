module View.Handlers where

import Data.Maybe (fromMaybe)
import qualified Model.Group as MG
import qualified Model.Project as MP
import qualified Model.Task as MT
import State.App (AppState (..), focusedGroup)
import State.Cursor (Cursor (..), isEmpty)
import Utils (Index, insertByIndex, limitListIndex, nextListIndex, prevListIndex, removeByIndex, swapIndices)
import View.Event (ViewBoardEvent (..))

boardHandler :: ViewBoardEvent -> AppState -> AppState
boardHandler PreviousTask state = changeTaskFocusIndex False state
boardHandler NextTask state = changeTaskFocusIndex True state
boardHandler PreviousGroup state = changeGroupFocusIndex False state
boardHandler NextGroup state = changeGroupFocusIndex True state
boardHandler Select state = selectCursor state
boardHandler MoveBackTask state = orderTaskIndex False state
boardHandler MoveNextTask state = orderTaskIndex True state
boardHandler MoveTaskPrevGroup state = moveTaskInGroups False state
boardHandler MoveTaskNextGroup state = moveTaskInGroups True state
boardHandler _ state = state

-- moveTaskInGroups
selectCursor :: AppState -> AppState
selectCursor state = if isEmpty (selectedCursor state) then state {selectedCursor = focusCursor state} else state {selectedCursor = Empty}

changeTaskFocusIndex :: Bool -> AppState -> AppState
changeTaskFocusIndex isNextBehaviour state = case focusCursor state of
  Empty -> state
  GroupI gIdx -> state {focusCursor = newFocusCursor (gIdx, Nothing)}
  TaskI gIdx tIdx -> state {focusCursor = newFocusCursor (gIdx, Just tIdx)}
  where
    orderStrategy :: (Index -> [a] -> Index)
    orderStrategy = if isNextBehaviour then nextListIndex else prevListIndex

    newFocusCursor :: (Int, Index) -> Cursor
    newFocusCursor (gIdx, taskI) = case focusedGroup state of
      Nothing -> Empty
      Just group -> case orderStrategy taskI (MG.groupTasks group) of
        Nothing -> GroupI gIdx
        Just i -> TaskI gIdx i

changeGroupFocusIndex :: Bool -> AppState -> AppState
changeGroupFocusIndex isNextBehaviour state = case focusCursor state of
  Empty -> state
  GroupI gIdx -> state {focusCursor = newFocusCursor gIdx}
  TaskI gIdx tIdx -> state {focusCursor = transferTaskIdx (newFocusCursor gIdx) tIdx}
  where
    orderStrategy :: (Index -> [a] -> Index)
    orderStrategy = if isNextBehaviour then nextListIndex else prevListIndex

    transferTaskIdx :: Cursor -> Int -> Cursor
    transferTaskIdx (GroupI gIdx) taskIdx = case limitListIndex taskIdx (MG.groupTasks (MP.getGroup' gIdx (project state))) of
      Nothing -> GroupI gIdx
      Just i -> TaskI gIdx i
    transferTaskIdx c _ = c

    newFocusCursor :: Int -> Cursor
    newFocusCursor gIdx =
      maybe
        Empty
        GroupI
        (orderStrategy (Just gIdx) (MP.projectGroups (project state)))

orderTaskIndex :: Bool -> AppState -> AppState
orderTaskIndex isNextBehaviour state = case selectedCursor state of
  (TaskI gIdx tIdx) -> case orderStrategy (Just tIdx) (MG.groupTasks (MP.getGroup' gIdx (project state))) of
    Nothing -> state
    Just newTIdx -> state {project = updateTaskIndex gIdx (tIdx, newTIdx) (project state), selectedCursor = TaskI gIdx newTIdx, focusCursor = TaskI gIdx newTIdx}
  _ -> state
  where
    orderStrategy :: (Index -> [a] -> Index)
    orderStrategy = if isNextBehaviour then nextListIndex else prevListIndex

    updateTaskIndex :: Int -> (Int, Int) -> MP.Project -> MP.Project
    updateTaskIndex gIdx (oldIdx, newIdx) = MP.modifyGroup gIdx (\group -> group {MG.groupTasks = swapIndices oldIdx newIdx (MG.groupTasks group)})

moveTaskInGroups :: Bool -> AppState -> AppState
moveTaskInGroups isNextBehaviour state = case selectedCursor state of
  (TaskI gIdx tIdx) -> case newGroupIdx gIdx of
    Nothing -> state
    Just newIdx ->
      let (currentGIdx, currentTIdx) = (gIdx, tIdx)
          currentTask = MG.groupTasks (MP.getGroup' currentGIdx (project state)) !! currentTIdx
          (newGIdx, newTIdx) = (newIdx, newTaskIdx (newIdx, currentTIdx))
          updatedProject = insertTask (newGIdx, newTIdx) currentTask $ removeTask (currentGIdx, currentTIdx) (project state)
       in state {project = updatedProject, focusCursor = TaskI newGIdx newTIdx, selectedCursor = TaskI newGIdx newTIdx}
  _ -> state
  where
    orderStrategy :: (Index -> [a] -> Index)
    orderStrategy = if isNextBehaviour then nextListIndex else prevListIndex

    newGroupIdx :: Int -> Index
    newGroupIdx i = orderStrategy (Just i) (MP.projectGroups (project state))

    newTaskIdx :: (Int, Int) -> Int
    newTaskIdx (gIdx, tIdx) = fromMaybe 0 (limitListIndex tIdx (MG.groupTasks (MP.getGroup' gIdx (project state))))

    removeTask :: (Int, Int) -> MP.Project -> MP.Project
    removeTask (gIdx, tIdx) = MP.modifyGroup gIdx (\g -> MG.withTasks (removeByIndex tIdx (MG.groupTasks g)) g)

    insertTask :: (Int, Int) -> MT.Task -> MP.Project -> MP.Project
    insertTask (gIdx, tIdx) task = MP.modifyGroup gIdx (\g -> MG.withTasks (insertByIndex tIdx task (MG.groupTasks g)) g)

-- moveTaskInGroups isNextBehaviour state = case selectedCursor state of
--   (TaskI gIdx tIdx) -> case newGroupIdx gIdx of
--     Nothing -> state
--     Just i ->
--       let newCursor = limitTaskIndex (i, tIdx)
--        in state {focusCursor = newCursor, selectedCursor = newCursor}
--   _ -> state
--   where
--     orderStrategy :: (Index -> [a] -> Index)
--     orderStrategy = if isNextBehaviour then nextListIndex else prevListIndex

--     newGroupIdx :: Int -> Index
--     newGroupIdx gIdx = orderStrategy (Just gIdx) (MP.projectGroups (project state))

--     limitTaskIndex :: (Int, Int) -> Cursor
--     limitTaskIndex (gIdx, tIdx) = case limitListIndex tIdx (MG.groupTasks (MP.getGroup' gIdx (project state))) of
--       Nothing -> GroupI gIdx
--       Just i -> TaskI gIdx i

-- Get (gIdx, tIdx) - (gIdx, tIdx)

-- 1. Get the new GroupIdx (Nothing -> state)
-- 2. Get the limited task index

-- 3. (oldG, oldT) (newG, newT)
