module Event where

import Brick (BrickEvent (..))
import qualified Graphics.Vty as V
import State.App (AppState (..))
import View.Event (BoardEvent (..), ViewEvent (..))

intoBoardEvent :: BrickEvent () e -> Maybe BoardEvent
intoBoardEvent (VtyEvent e) = case e of
  V.EvKey V.KRight [] -> Just MoveRight
  V.EvKey V.KLeft [] -> Just MoveLeft
  V.EvKey V.KDown [] -> Just MoveDown
  V.EvKey V.KUp [] -> Just MoveUp
  V.EvKey V.KEnter [] -> Just SelectItem
  _ -> Nothing
intoBoardEvent _ = Nothing

intoViewEvent :: BrickEvent () e -> AppState -> ViewEvent
intoViewEvent e _state = maybe Void Board (intoBoardEvent e)
