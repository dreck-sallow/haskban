module View.Event where

-- data ViewBoardEvent = PreviousTask | NextTask | PreviousGroup | NextGroup | Select | MoveBackTask | MoveNextTask | MoveTaskPrevGroup | MoveTaskNextGroup | Void

data FormEvent = Submit | Cancel

data BoardEvent = MoveUp | MoveDown | MoveLeft | MoveRight | SelectItem

data ViewEvent = Board BoardEvent | Form FormEvent | Quit | Void
