{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Storage (loadStorageMapping, saveStoreMapping, loadProjectId, saveNewProject, loadData, saveData) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Directory (storageFilePath)
import GHC.Generics
import System.IO.Error (catchIOError)
import Util.Date (currentTimestamp)

data StoreMapping where
  StoreMapping :: {projects :: Map.Map String String} -> StoreMapping
  deriving (Generic, Show)

instance ToJSON StoreMapping

instance FromJSON StoreMapping

-- file functions for save & read + parse file
saveData :: (ToJSON a) => FilePath -> a -> IO ()
saveData filePath obj = BL.writeFile filePath (encode obj)

loadData :: (FromJSON a) => FilePath -> IO (Maybe a)
loadData filePath = do
  contents <- BL.readFile filePath
  return $ decode contents

loadStorageMapping :: IO (Maybe StoreMapping)
loadStorageMapping = do
  storeFile <- storageFilePath
  -- catchIOError (loadData storeFile) (const $ return Nothing)
  catchIOError
    (loadData storeFile)
    ( \e -> do
        print e
        return Nothing
    )

saveStoreMapping :: StoreMapping -> IO ()
saveStoreMapping storeMapping = do
  storeFile <- storageFilePath
  saveData storeFile storeMapping

loadProjectId :: String -> IO (Maybe String)
loadProjectId projectName = do
  content <- loadStorageMapping
  case content of
    Nothing -> return Nothing
    Just (StoreMapping p) -> return $ Map.lookup projectName p

saveNewProject :: String -> IO ()
saveNewProject projectName = do
  content <- loadStorageMapping
  timestamp <- currentTimestamp
  case content of
    Nothing -> return ()
    Just (StoreMapping p) -> saveStoreMapping $ StoreMapping (Map.insert projectName (show timestamp) p)
