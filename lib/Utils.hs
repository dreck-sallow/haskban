module Utils where

import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0 ..]

swapIndices :: Int -> Int -> [a] -> [a]
swapIndices _ _ [] = []
swapIndices j k list
  | j == k = list
  | j >= length list || k >= length list = list
  | otherwise = zipWith swap' [0 ..] list
  where
    swap' i itm
      | i == j = list !! k
      | i == k = list !! j
      | otherwise = itm

-- TODO: make more efficient replace breaking the loop when found the idx to set
replaceByIndex :: Int -> a -> [a] -> [a]
replaceByIndex _ _ [] = []
replaceByIndex idx itm list
  | idx < 0 && idx >= length list = list
  | otherwise = zipWith replace' [0 ..] list
  where
    replace' i currentItm = if i == idx then itm else currentItm

-- No manage the index out of bounds
insertByIndex :: Int -> a -> [a] -> [a]
insertByIndex 0 itm list = itm : list
insertByIndex idx itm list
  | idx >= length list = list <> [itm]
  | otherwise =
      let (x, xs) = splitAt idx list
       in x <> [itm] <> xs

removeByIndex :: Int -> [a] -> [a]
removeByIndex _ [] = []
removeByIndex idx list
  | idx > (length list - 1) || idx < 0 = list
  | otherwise = take idx list ++ drop (idx + 1) list

currentTimestamp :: IO Int
currentTimestamp = do
  currentTime <- getCurrentTime
  let timestamp = utcTimeToPOSIXSeconds currentTime
  return $ round timestamp
