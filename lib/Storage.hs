{-# LANGUAGE DeriveGeneric #-}

module Storage where

import GHC.Generics
import qualified Data.Map.Strict as Map
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Data.ByteString.Lazy as BL

data StoreMapping = StoreMapping 
  {
    projects :: Map.Map String String
  } deriving (Generic)


instance ToJSON StoreMapping
instance FromJSON StoreMapping

saveData :: ToJSON a => FilePath -> a -> IO ()
saveData filePath obj = BL.writeFile filePath (encode obj)

loadData :: FromJSON a => FilePath -> IO (Maybe a)
loadData filePath = do
  contents <- BL.readFile filePath
  return $ decode contents
