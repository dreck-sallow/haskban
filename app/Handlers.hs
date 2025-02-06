module Handlers where

import State.Program

data StateEvent = NextTask | PrevTask | NextGroup | PrevGroup

mainHandler :: StateEvent -> ProgramState -> ProgramState
mainHandler NextTask = nextTaskHandler
mainHandler PrevTask = prevTaskHandler
mainHandler NextGroup = nextGroupHandler
mainHandler PrevGroup = prevGroupHandler

-- Defined handlers

nextTaskHandler :: ProgramState -> ProgramState
nextTaskHandler state = state {focusCursor = nextTask (focusCursor state) (project state)}

prevTaskHandler :: ProgramState -> ProgramState
prevTaskHandler state = state {focusCursor = prevTask (focusCursor state) (project state)}

nextGroupHandler :: ProgramState -> ProgramState
nextGroupHandler state = state {focusCursor = nextGroup (focusCursor state) (project state)}

prevGroupHandler :: ProgramState -> ProgramState
prevGroupHandler state = state {focusCursor = prevGroup (focusCursor state) (project state)}
