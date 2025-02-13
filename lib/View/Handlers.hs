module View.Handlers where

import qualified Model.Group as MG
import qualified Model.Project as MP
import State.App (AppState (..), focusedGroup)
import State.Cursor (Cursor (..), isEmpty)
import Utils (Index, limitListIndex, nextListIndex, prevListIndex, swapIndices)
import View.Event (ViewBoardEvent (..))

boardHandler :: ViewBoardEvent -> AppState -> AppState
boardHandler PreviousTask state = changeTaskFocusIndex False state
boardHandler NextTask state = changeTaskFocusIndex True state
boardHandler PreviousGroup state = changeGroupFocusIndex False state
boardHandler NextGroup state = changeGroupFocusIndex True state
boardHandler Select state = selectCursor state
boardHandler MoveBackTask state = orderTaskIndex False state
boardHandler MoveNextTask state = orderTaskIndex True state
boardHandler MoveTaskPrevGroup state = moveTaskInGroups True state
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
    Just i ->
      let newCursor = limitTaskIndex (i, tIdx)
       in state {focusCursor = newCursor, selectedCursor = newCursor}
  _ -> state
  where
    orderStrategy :: (Index -> [a] -> Index)
    orderStrategy = if isNextBehaviour then nextListIndex else prevListIndex

    newGroupIdx :: Int -> Index
    newGroupIdx gIdx = orderStrategy (Just gIdx) (MP.projectGroups (project state))

    limitTaskIndex :: (Int, Int) -> Cursor
    limitTaskIndex (gIdx, tIdx) = case limitListIndex tIdx (MG.groupTasks (MP.getGroup' gIdx (project state))) of
      Nothing -> GroupI gIdx
      Just i -> TaskI gIdx i
