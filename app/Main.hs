module Main where

import Brick (modify)
import qualified Brick.Main as Main
import Brick.Types (BrickEvent (..), EventM)
import qualified Cli
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Event (intoBoardEvent)
import qualified Graphics.Vty as V
import Handlers
import qualified Model.Project as MP
import Project (loadProject)
import State.App (AppState (..))
import State.Cursor (Cursor (..))
import Storage (loadProjectId)
import UI (drawUI, styleApp)
import Utils (currentTimestamp)
import View.Handlers (boardHandler)

loadProject' :: String -> IO (Maybe MP.Project)
loadProject' projectName = do
  projectId' <- loadProjectId projectName
  case projectId' of
    Nothing -> return Nothing
    Just id' -> loadProject id'

appHandleEvent' :: BrickEvent () e -> EventM () AppState ()
appHandleEvent' (VtyEvent e) = case e of
  V.EvKey (V.KChar 'q') [] -> Main.halt
  _ -> case intoBoardEvent (VtyEvent e) of
    Nothing -> return ()
    Just ev -> modify (boardHandler ev)
appHandleEvent' e = case intoBoardEvent e of
  Nothing -> return ()
  Just ev -> modify (boardHandler ev)

programApp :: Main.App AppState e ()
programApp =
  Main.App
    { Main.appDraw = drawUI,
      Main.appChooseCursor = Main.neverShowCursor,
      Main.appHandleEvent = appHandleEvent',
      Main.appStartEvent = return (),
      Main.appAttrMap = const styleApp
    }

initalState :: MP.Project -> AppState
initalState project' =
  let focusCursor = if null (MP.projectGroups project') then Empty else GroupI 0
   in AppState
        { project = project',
          focusCursor = GroupI 0,
          selectedCursor = Empty
        }

runApp :: Cli.CliOptions -> IO ()
runApp (Cli.CliOptions p) = do
  _project' <- loadProjectByName' p
  case _project' of
    Nothing -> print ("The project " ++ fromMaybe "" p ++ " not found")
    Just _p -> void $ Main.defaultMain programApp (initalState _p)
  where
    defaultProject' :: IO MP.Project
    defaultProject' = do
      projectId <- currentTimestamp
      return
        MP.Project
          { MP.projectId = projectId,
            MP.projectName = show projectId,
            MP.projectGroups = []
          }
    loadProjectByName' :: Maybe String -> IO (Maybe MP.Project)
    loadProjectByName' Nothing = Just <$> defaultProject'
    loadProjectByName' (Just n) = loadProject' n

main :: IO ()
main = do
  cliOpts <- Cli.parseToCli
  runApp cliOpts
