module View.Handlers where

-- import Model.Project (Project (..))
import qualified Model.Group as MG
import State.App (AppState (..), focusedGroup)
import State.Cursor (Cursor (..), isEmpty)
import Utils (Index, nextListIndex, prevListIndex)
import View.Event (ViewBoardEvent (..))

boardHandler :: ViewBoardEvent -> AppState -> AppState
boardHandler PreviousTask state = changeTaskFocusIndex False state
boardHandler NextTask state = changeTaskFocusIndex True state
boardHandler Select state = selectCursor state

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
