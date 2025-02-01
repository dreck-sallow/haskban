{-# LANGUAGE DeriveGeneric #-}

module Storage (loadStorageMapping, saveStoreMapping, loadProjectId, saveNewProject) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Generics
import System.IO.Error (catchIOError)

data StoreMapping = StoreMapping
  { projects :: Map.Map String String
  }
  deriving (Generic, Show)

instance ToJSON StoreMapping

instance FromJSON StoreMapping

saveData :: (ToJSON a) => FilePath -> a -> IO ()
saveData filePath obj = BL.writeFile filePath (encode obj)

loadData :: (FromJSON a) => FilePath -> IO (Maybe a)
loadData filePath = do
  contents <- BL.readFile filePath
  return $ decode contents

loadStorageMapping :: IO (Maybe StoreMapping)
loadStorageMapping = catchIOError (loadData "storage.json") (const $ return Nothing) -- FIXME: use the path to local system path

saveStoreMapping :: StoreMapping -> IO ()
saveStoreMapping = saveData "storage.json" -- FIXME: use the path to local system path

loadProjectId :: String -> IO (Maybe String)
loadProjectId projectName = do
  content <- loadStorageMapping
  case content of
    Nothing -> return Nothing
    Just (StoreMapping p) -> return $ Map.lookup projectName p

saveNewProject :: String -> IO ()
saveNewProject projectName = do
  content <- loadStorageMapping
  timestamp <- currentTimeToInt
  case content of
    Nothing -> return ()
    Just (StoreMapping p) -> saveStoreMapping $ StoreMapping (Map.insert projectName (show timestamp) p)

--
-- Define utility functions for storage functionality
--

currentTimeToInt :: IO Int
currentTimeToInt = do
  currentTime <- getCurrentTime
  let timestamp = utcTimeToPOSIXSeconds currentTime
  return $ round timestamp

-- Ww need get the default data dir cross-operating-system
