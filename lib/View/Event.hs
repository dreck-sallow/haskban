module View.Event where

data ViewBoardEvent = PreviousTask | NextTask | PreviousGroup | NextGroup | Select | MoveBackTask | MoveNextTask | MoveTaskPrevGroup | MoveTaskNextGroup | Void

data ViewFormEvent = Submit | Cancel
