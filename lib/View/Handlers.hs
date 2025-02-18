module View.Handlers where

import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import qualified Model.Group as MG
import qualified Model.Project as MP
import qualified Model.Task as MT
import State.App (AppState (..), getTasksByGroup, setFocus, setProject, setSelected)
import State.Cursor (Cursor (..), isEmpty, isTaskI)
import Util.List (Movement (..), isPrevMove, moveIndex, nextIndex, prevIndex)
import Utils (Index, insertByIndex, limitListIndex, nextListIndex, prevListIndex, removeByIndex, swapIndices)
import View.Event (BoardEvent (..))

boardHandler :: BoardEvent -> AppState -> AppState
boardHandler MoveUp state = if isTaskI (selectedCursor state) then orderTaskIndex Prev state else changeTaskFocusIndex Prev state
boardHandler MoveDown state = if isTaskI (selectedCursor state) then orderTaskIndex Next state else changeTaskFocusIndex Next state
boardHandler MoveLeft state = changeGroupFocusIndex Prev state
boardHandler MoveRight state = changeGroupFocusIndex Next state
boardHandler SelectItem state = selectCursor state

-- boardHandler _ state = state

-- moveTaskInGroups
selectCursor :: AppState -> AppState
selectCursor state = if isEmpty (selectedCursor state) then state {selectedCursor = focusCursor state} else state {selectedCursor = Empty}

changeTaskFocusIndex :: Movement -> AppState -> AppState
changeTaskFocusIndex mov state = case focusCursor state of
  Empty -> state
  GroupI gIdx -> setFocus (newFocusCursor (gIdx, Nothing)) state
  TaskI gIdx tIdx -> setFocus (newFocusCursor (gIdx, Just tIdx)) state
  where
    newFocusCursor :: (Int, Index) -> Cursor
    newFocusCursor (gI, tI) = case tI of
      Nothing ->
        if isPrevMove mov
          then GroupI gI
          else bool (GroupI gI) (TaskI gI 0) (not . null $ getTasksByGroup gI state)
      Just i ->
        if isPrevMove mov
          then maybe (GroupI gI) (TaskI gI) (prevIndex i (getTasksByGroup gI state))
          else maybe (TaskI gI i) (TaskI gI) (nextIndex i (getTasksByGroup gI state))

changeGroupFocusIndex :: Movement -> AppState -> AppState
changeGroupFocusIndex mov state = case focusCursor state of
  Empty -> state
  GroupI gIdx -> setFocus (maybe (GroupI gIdx) GroupI (moveGroup gIdx)) state
  TaskI gI tI -> setFocus (applyTaskMove (gI, tI)) state
  where
    moveStrategy :: (Int -> [a] -> Maybe Int)
    moveStrategy = moveIndex mov

    moveGroup :: Int -> Maybe Int
    moveGroup i = moveStrategy i (MP.projectGroups (project state))

    applyTaskMove :: (Int, Int) -> Cursor
    applyTaskMove (g, t) = case moveGroup g of
      Nothing -> GroupI g
      Just i -> TaskI i (boundTaskIdx t (getTasksByGroup i state))

    boundTaskIdx :: Int -> [MT.Task] -> Int
    boundTaskIdx i list
      | i > (length list - 1) = length list - 1
      | otherwise = i

orderTaskIndex :: Movement -> AppState -> AppState
orderTaskIndex mov state = case selectedCursor state of
  (TaskI gIdx tIdx) -> case moveStrategy tIdx (MG.groupTasks (MP.getGroup' gIdx (project state))) of
    Nothing -> state
    Just i -> setProject (updateTaskIndex gIdx (tIdx, i)) (setSelected (TaskI gIdx i) state)
  _ -> state
  where
    moveStrategy :: (Int -> [a] -> Maybe Int)
    moveStrategy = moveIndex mov

    updateTaskIndex :: Int -> (Int, Int) -> MP.Project -> MP.Project
    updateTaskIndex gIdx (oldIdx, newIdx) = MP.modifyGroup gIdx (MG.setTasks (swapIndices oldIdx newIdx))

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
