module Main where

import Brick (Widget, joinBorders, str, withBorderStyle, (<+>))
import Brick.AttrMap (AttrMap, attrMap)
import qualified Brick.Main as Main
import Brick.Types (BrickEvent(..), EventM)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import qualified Models as M
import Project (loadProject)
import Storage (loadProjectId)

-- import qualified MyLib (someFunc)

data ProgramState = ProgramState {project :: M.Project}

drawUI :: ProgramState -> [Widget ()]
drawUI _state = [joinBorders $ withBorderStyle unicode $ borderWithLabel (str "Hello!") (center (str "left") <+> vBorder <+> center (str "Right"))]

loadProject' :: String -> IO M.Project
loadProject' projectName = do
  projectId' <- loadProjectId projectName
  project' <- loadProject (fromMaybe "" projectId')
  case project' of
    Nothing -> return M.defaultProject
    Just p -> return p

appEvent :: BrickEvent () e -> EventM () ProgramState ()
appEvent _ = return ()
appEvent (VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'q') [] -> Main.halt
    _ -> return ()

theMap :: AttrMap
theMap = attrMap V.defAttr []

programApp :: Main.App ProgramState e ()
programApp =
  Main.App
    { Main.appDraw = drawUI,
      Main.appChooseCursor = Main.neverShowCursor,
      Main.appHandleEvent = appEvent,
      Main.appStartEvent = return (),
      Main.appAttrMap = const theMap
    }

initalState :: ProgramState
initalState = ProgramState {project = M.defaultProject}

main :: IO ()
main = do
  _project' <- loadProject' "test"
  void $ Main.defaultMain programApp initalState
