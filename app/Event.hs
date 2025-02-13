module Event where

import Brick (BrickEvent (..))
import qualified Graphics.Vty as V
import State.App (AppState (..))
import State.Cursor (isEmpty, isTaskI)
import View.Event (ViewBoardEvent (..))

intoBoardEvent :: BrickEvent () e -> AppState -> ViewBoardEvent
intoBoardEvent (VtyEvent e) state = case e of
  V.EvKey V.KRight [] -> if isEmpty (selectedCursor state) then NextGroup else MoveTaskNextGroup
  V.EvKey V.KLeft [] -> if isEmpty (selectedCursor state) then PreviousGroup else MoveTaskPrevGroup
  V.EvKey V.KDown [] -> if isTaskI (selectedCursor state) then MoveNextTask else NextTask
  V.EvKey V.KUp [] -> if isTaskI (selectedCursor state) then MoveBackTask else PreviousTask
  V.EvKey V.KEnter [] -> Select
  _ -> Void
intoBoardEvent _ _ = Void
