module Main where

import Brick (modify)
import qualified Brick.Main as Main
import Brick.Types (BrickEvent (..), EventM)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import Handlers
import qualified Models as M
import Project (loadProject)
import State.FocusIndex (Cursor (..))
import State.Program (ProgramState (..))
import Storage (loadProjectId)
import View (drawUI, styleApp)

loadProject' :: String -> IO M.Project
loadProject' projectName = do
  projectId' <- loadProjectId projectName
  project' <- loadProject (fromMaybe "" projectId')
  case project' of
    Nothing -> return M.defaultProject
    Just p -> return p

appEvent :: BrickEvent () e -> EventM () ProgramState ()
appEvent (VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'q') [] -> Main.halt
    V.EvKey V.KRight [] -> modify (mainHandler NextGroup)
    V.EvKey V.KLeft [] -> modify (mainHandler PrevGroup)
    V.EvKey V.KDown [] -> modify (mainHandler NextTask)
    V.EvKey V.KUp [] -> modify (mainHandler PrevTask)
    _ -> return ()
appEvent _ = return ()

programApp :: Main.App ProgramState StateEvent ()
programApp =
  Main.App
    { Main.appDraw = drawUI,
      Main.appChooseCursor = Main.neverShowCursor,
      Main.appHandleEvent = appEvent,
      Main.appStartEvent = return (),
      Main.appAttrMap = const styleApp
    }

getColumnsCount :: Int
getColumnsCount = 4

initalState :: ProgramState
initalState =
  ProgramState
    { project =
        M.Project
          { M.projectId = 1,
            M.projectName = "2",
            M.projectGroups =
              [ M.Group
                  { M.groupName = "Grupo 1",
                    M.groupTasks =
                      [ M.Task
                          { M.taskId = 1,
                            M.taskTitle = "Tarea 1",
                            M.taskDesc =
                              "Tarea del grupo numero 1",
                            M.taskEndDate = Nothing
                          },
                        M.Task
                          { M.taskId = 2,
                            M.taskTitle = "Tarea 2",
                            M.taskDesc =
                              "Tarea del grupo numero 2",
                            M.taskEndDate = Nothing
                          }
                      ]
                  },
                M.Group
                  { M.groupName = "Grupo 2",
                    M.groupTasks =
                      [ M.Task
                          { M.taskId = 2,
                            M.taskTitle = "Tarea 2",
                            M.taskDesc =
                              "Tarea del grupo numero 2",
                            M.taskEndDate = Nothing
                          },
                        M.Task
                          { M.taskId = 2,
                            M.taskTitle = "Tarea 2",
                            M.taskDesc =
                              "Tarea del grupo numero 2",
                            M.taskEndDate = Nothing
                          }
                      ]
                  }
              ]
          },
      focusCursor =
        GroupI 0
    }

main :: IO ()
main = do
  _project' <- loadProject' "test"
  void $ Main.defaultMain programApp initalState
