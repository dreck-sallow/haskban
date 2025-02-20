module View.Handlers where

import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import qualified Model.Group as MG
import qualified Model.Project as MP
import qualified Model.Task as MT
import State.App (AppState (..), getTasksByGroup, setFocus, setProject, setSelected)
import State.Cursor (Cursor (..), isEmpty, isTaskI)
import Util.List (Movement (..), insertByIndex, isPrevMove, limitIndexOrLast, moveIndex, nextIndex, prevIndex, removeByIndex, swapIndices)
import View.Event (BoardEvent (..))

boardHandler :: BoardEvent -> AppState -> AppState
boardHandler MoveUp state = if isTaskI (selectedCursor state) then orderTaskIndex Prev state else changeTaskFocusIndex Prev state
boardHandler MoveDown state = if isTaskI (selectedCursor state) then orderTaskIndex Next state else changeTaskFocusIndex Next state
boardHandler MoveLeft state = if isEmpty (selectedCursor state) then changeGroupFocusIndex Prev state else moveTaskInGroups Prev state
boardHandler MoveRight state = if isEmpty (selectedCursor state) then changeGroupFocusIndex Next state else moveTaskInGroups Next state
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
    newFocusCursor :: (Int, Maybe Int) -> Cursor
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
    updateTaskIndex gIdx (oldIdx, newIdx) = MP.modifyGroup gIdx (MG.setTasks (swapIndices (oldIdx, newIdx)))

moveTaskInGroups :: Movement -> AppState -> AppState
moveTaskInGroups mov state = case selectedCursor state of
  (TaskI g1 t1) -> case moveStrategy g1 (MP.projectGroups (project state)) of
    Nothing -> state
    Just g2 ->
      let t2 = fromMaybe 0 (limitIndexOrLast t1 (MG.groupTasks (MP.getGroup' g2 (project state))))
       in setSelected (TaskI g2 t2) $ setProject (moveTask (g1, t1) (g2, t2)) state
  (GroupI gIdx) -> case moveStrategy gIdx (MP.projectGroups (project state)) of
    Nothing -> state
    Just i -> setSelected (GroupI i) $ setProject (MP.swapGroupByIndex (gIdx, i)) state
  _ -> state
  where
    moveStrategy :: (Int -> [a] -> Maybe Int)
    moveStrategy = moveIndex mov

    moveTask :: (Int, Int) -> (Int, Int) -> MP.Project -> MP.Project
    moveTask (g1, t1) (g2, t2) p =
      let taskToMove = MG.groupTasks (MP.getGroup' g1 p) !! t1
       in MP.modifyGroup g1 (MG.setTasks (removeByIndex t1)) $ MP.modifyGroup g2 (MG.setTasks (insertByIndex t2 taskToMove)) p
