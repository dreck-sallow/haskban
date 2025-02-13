module UI where

import Brick
import Brick.Widgets.Border (border, borderWithLabel)
import Data.Maybe (isNothing)
import Graphics.Vty (defAttr)
import Graphics.Vty.Attributes (red)
import qualified Model.Group as MG
import qualified Model.Project as MP
import qualified Model.Task as MT
import State.App (AppState (..))
import qualified State.App as SA
import State.Cursor (Cursor (..))
import Utils (Index)

drawUI :: SA.AppState -> [Widget ()]
drawUI state = [hBox $ mapWithIndex (\i g -> renderGroup g (isCurrentGroup i, taskIdx)) (MP.projectGroups (project state))]
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

renderGroup :: MG.Group -> (Bool, Index) -> Widget ()
renderGroup group (currentGroup, taskI) = if currentGroup && isNothing taskI then withAttr focusedAttrName drawGroup else drawGroup
  where
    checkFocusTask :: Int -> Bool
    checkFocusTask i = Just i == taskI && currentGroup

    drawGroup :: Widget ()
    drawGroup =
      borderWithLabel (str (MG.groupName group)) $
        vBox $
          mapWithIndex (\i t -> renderTask t (checkFocusTask i)) (MG.groupTasks group)

renderTask :: MT.Task -> Bool -> Widget ()
renderTask task isFocused = if isFocused then withAttr focusedAttrName drawTask else drawTask
  where
    drawTask :: Widget ()
    drawTask =
      border $
        vBox
          [ strWrap (MT.taskTitle task),
            strWrap $ take 20 (MT.taskContent task) -- take a short description of task
          ]

focusedAttrName :: AttrName
focusedAttrName = attrName "focus"

styleApp :: AttrMap
styleApp =
  attrMap
    defAttr
    [ (focusedAttrName, fg red)
    ]
