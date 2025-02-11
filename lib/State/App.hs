module State.App where

import qualified Model.Group as MG
import qualified Model.Project as MP
import qualified Model.Task as MT
import State.Cursor (Cursor (..))

data CursorItem = GroupItem MG.Group | TaskItem MT.Task

data AppState = AppState
  { project :: MP.Project,
    focusCursor :: Cursor,
    selectedCursor :: Cursor
  }

focusedItem :: AppState -> Maybe CursorItem
focusedItem state = projectCursorItem (focusCursor state) (project state)

selectedItem :: AppState -> Maybe CursorItem
selectedItem state = projectCursorItem (selectedCursor state) (project state)

focusedGroup :: AppState -> Maybe MG.Group
focusedGroup state = case focusCursor state of
  Empty -> Nothing
  GroupI gIdx -> Just $ MP.projectGroups (project state) !! gIdx
  TaskI gIdx _ -> Just $ MP.projectGroups (project state) !! gIdx

projectCursorItem :: Cursor -> MP.Project -> Maybe CursorItem
projectCursorItem cursor project' = case cursor of
  Empty -> Nothing
  GroupI gIdx -> GroupItem <$> MP.getGroup gIdx project'
  TaskI gIdx tIdx -> case MP.getGroup gIdx project' of
    Nothing -> Nothing
    Just p -> TaskItem <$> MG.getTask tIdx p
