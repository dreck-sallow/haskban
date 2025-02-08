module Main where

import Brick (modify)
import qualified Brick.Main as Main
import Brick.Types (BrickEvent (..), EventM)
import qualified Cli
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import Handlers
import qualified Models as M
import Project (loadProject)
import State.FocusIndex (Cursor (..))
import State.Program (ProgramState (..))
import Storage (loadProjectId)
import Utils (currentTimestamp)
import View (drawUI, styleApp)

loadProject' :: String -> IO (Maybe M.Project)
loadProject' projectName = do
  projectId' <- loadProjectId projectName
  case projectId' of
    Nothing -> return Nothing
    Just id' -> loadProject id'

appEvent :: BrickEvent () e -> EventM () ProgramState ()
appEvent (VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'q') [] -> Main.halt
    V.EvKey V.KRight [] -> modify (mainHandler KeyRight)
    V.EvKey V.KLeft [] -> modify (mainHandler KeyLeft)
    V.EvKey V.KDown [] -> modify (mainHandler KeyDown)
    V.EvKey V.KUp [] -> modify (mainHandler KeyUp)
    V.EvKey V.KEnter [] -> modify (mainHandler Enter)
    _ -> return ()
appEvent _ = return ()

programApp :: Main.App ProgramState EventCmd ()
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

initalState :: M.Project -> ProgramState
initalState project' =
  let focusCursor = if null (M.projectGroups project') then Empty else GroupI 0
   in ProgramState
        { project = project',
          focusCursor = GroupI 0,
          selected = Empty
        }

runApp :: Cli.CliOptions -> IO ()
runApp (Cli.CliOptions p) = do
  _project' <- loadProjectByName' p
  case _project' of
    Nothing -> print ("The project " ++ fromMaybe "" p ++ " not found")
    Just _p -> void $ Main.defaultMain programApp (initalState _p)
  where
    defaultProject' :: IO M.Project
    defaultProject' = do
      projectId <- currentTimestamp
      return
        M.Project
          { M.projectId = projectId,
            M.projectName = show projectId,
            M.projectGroups = []
          }
    loadProjectByName' :: Maybe String -> IO (Maybe M.Project)
    loadProjectByName' Nothing = Just <$> defaultProject'
    loadProjectByName' (Just n) = loadProject' n

main :: IO ()
main = do
  cliOpts <- Cli.parseToCli
  runApp cliOpts
