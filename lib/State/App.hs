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

setFocus :: Cursor -> AppState -> AppState
setFocus cursor state = state {focusCursor = cursor}

setSelected :: Cursor -> AppState -> AppState
setSelected cursor state = state {selectedCursor = cursor, focusCursor = cursor} -- TODO: check for another function for apply focusCursor

getProjectGroup :: Int -> AppState -> MG.Group
getProjectGroup i state = MP.projectGroups (project state) !! i

getTasksByGroup :: Int -> AppState -> [MT.Task]
getTasksByGroup i state = MG.groupTasks $ getProjectGroup i state

setProject :: (MP.Project -> MP.Project) -> AppState -> AppState
setProject fn state = state {project = fn (project state)}
