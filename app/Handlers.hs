module Handlers where

import qualified Models as M
import State.FocusIndex
import State.Program

-- data HandlerEvent = NextTask | PrevTask | NextGroup | PrevGroup | MoveBackTask

data EventCmd = KeyUp | KeyDown | KeyLeft | KeyRight | Enter

mainHandler :: EventCmd -> ProgramState -> ProgramState
mainHandler KeyUp state = if isTaskI (selected state) then moveBackTaskHandler state else prevTaskHandler state
mainHandler Enter state = if isEmpty (selected state) then state {selected = focusCursor state} else state {selected = Empty}
mainHandler KeyDown state = if isTaskI (selected state) then moveForwardTaskHandler state else nextTaskHandler state
mainHandler KeyLeft state = if isEmpty (selected state) then prevGroupHandler state else moveBackGroupHandler state
mainHandler KeyRight state = if isEmpty (selected state) then nextGroupHandler state else moveForwardGroupHandler state
-- mainHandler _ state = state

-- Defined handlers

nextTaskHandler :: ProgramState -> ProgramState
nextTaskHandler state = state {focusCursor = nextTask (focusCursor state) (project state)}

prevTaskHandler :: ProgramState -> ProgramState
prevTaskHandler state = state {focusCursor = prevTask (focusCursor state) (project state)}

nextGroupHandler :: ProgramState -> ProgramState
nextGroupHandler state = state {focusCursor = nextGroup (focusCursor state) (project state)}

prevGroupHandler :: ProgramState -> ProgramState
prevGroupHandler state = state {focusCursor = prevGroup (focusCursor state) (project state)}

-- move selected cursor
moveBackTaskHandler :: ProgramState -> ProgramState
moveBackTaskHandler state = case selected state of
  (TaskI groupI taskI) ->
    let group = M.projectGroups (project state) !! groupI
        tasks = M.groupTasks group
        (newSelectedCursor, newTasks) = moveBackTask (TaskI groupI taskI) tasks
        updatedGroup = group {M.groupTasks = newTasks}
        updatedGroups = updateListAt groupI updatedGroup (M.projectGroups (project state))
        updateProject = (project state) {M.projectGroups = updatedGroups}
     in state {project = updateProject, focusCursor = newSelectedCursor, selected = newSelectedCursor}
  _ -> state

moveForwardTaskHandler :: ProgramState -> ProgramState
moveForwardTaskHandler state = case selected state of
  (TaskI groupI taskI) ->
    let group = M.projectGroups (project state) !! groupI
        tasks = M.groupTasks group
        (newSelectedCursor, newTasks) = moveForwardTask (TaskI groupI taskI) tasks
        updatedGroup = group {M.groupTasks = newTasks}
        updatedGroups = updateListAt groupI updatedGroup (M.projectGroups (project state))
        updateProject = (project state) {M.projectGroups = updatedGroups}
     in state {project = updateProject, focusCursor = newSelectedCursor, selected = newSelectedCursor}
  _ -> state

moveBackGroupHandler :: ProgramState -> ProgramState
moveBackGroupHandler state =
  let groups' = M.projectGroups $ project state
      (selectedCursor, updatedGroups) = moveBackGroup (selected state) groups'
      updateProject = (project state) {M.projectGroups = updatedGroups}
   in state {project = updateProject, focusCursor = selectedCursor, selected = selectedCursor}


moveForwardGroupHandler :: ProgramState -> ProgramState
moveForwardGroupHandler state =
  let groups' = M.projectGroups $ project state
      (selectedCursor, updatedGroups) = moveForwardGroup (selected state) groups'
      updateProject = (project state) {M.projectGroups = updatedGroups}
   in state {project = updateProject, focusCursor = selectedCursor, selected = selectedCursor}

updateListAt :: Int -> a -> [a] -> [a]
updateListAt _ _ [] = []
updateListAt 0 itm (_x : xs) = itm : xs
updateListAt idx itm (x : xs) = x : updateListAt (idx - 1) itm xs
