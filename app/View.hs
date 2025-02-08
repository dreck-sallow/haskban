module View where

import Brick
import Brick.Widgets.Border (border, borderWithLabel)
import Data.Maybe (isNothing)
import Graphics.Vty (defAttr)
import Graphics.Vty.Attributes (red)
import qualified Models as M
import State.FocusIndex (Cursor (GroupI, TaskI), Index)
import State.Program (ProgramState (..))
import qualified State.Program as SP

drawUI :: SP.ProgramState -> [Widget ()]
drawUI state = [hBox $ mapWithIndex (\i g -> renderGroup g (isCurrentGroup i, taskIdx)) (M.projectGroups (project state))]
  where
    isCurrentGroup :: Int -> Bool
    isCurrentGroup i = case focusCursor state of
      GroupI gI -> gI == i
      (TaskI gI _) -> gI == i
      _ -> False

    taskIdx :: Index
    taskIdx = case focusCursor state of
      (TaskI _groupI taskI) -> Just taskI
      _ -> Nothing

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0 ..]

renderGroup :: M.Group -> (Bool, Index) -> Widget ()
renderGroup group (currentGroup, taskI) = if currentGroup && isNothing taskI then withAttr focusedAttrName drawGroup else drawGroup
  where
    checkFocusTask :: Int -> Bool
    checkFocusTask i = Just i == taskI && currentGroup

    drawGroup :: Widget ()
    drawGroup =
      borderWithLabel (str (M.groupName group)) $
        vBox $
          mapWithIndex (\i t -> renderTask t (checkFocusTask i)) (M.groupTasks group)

renderTask :: M.Task -> Bool -> Widget ()
renderTask task isFocused = if isFocused then withAttr focusedAttrName drawTask else drawTask
  where
    drawTask :: Widget ()
    drawTask =
      border $
        vBox
          [ strWrap (M.taskTitle task),
            strWrap $ take 20 (M.taskContent task) -- take a short description of task
          ]

focusedAttrName :: AttrName
focusedAttrName = attrName "focus"

styleApp :: AttrMap
styleApp =
  attrMap
    defAttr
    [ (focusedAttrName, fg red)
    ]
