module View.Event where

data ViewBoardEvent = PreviousTask | NextTask | PreviousGroup | NextGroup | Select | MoveTaskPrevGroup | MoveTaskNextGroup

data ViewFormEvent = Submit | Cancel
