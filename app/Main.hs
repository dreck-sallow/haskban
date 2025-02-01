module Main where

import Storage (loadStorageMapping)

-- import qualified MyLib (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  storeMapping <- loadStorageMapping
  print storeMapping
