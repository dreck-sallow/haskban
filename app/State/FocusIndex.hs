module State.FocusIndex where

import Data.Maybe (isNothing)

type Index = Maybe Int

data Cursor = Empty | GroupI Int | TaskI Int Int

nextListIndex :: Index -> [a] -> Index
nextListIndex _ [] = Nothing
nextListIndex index list
  | maybe False (>= length list - 1) index = Just $ length list - 1
  | otherwise = succ <$> index

prevListIndex :: Index -> [a] -> Index
prevListIndex _ [] = Nothing
prevListIndex index list
  | Just 0 == index = Just 0
  | maybe False (> length list - 1) index = Just $ length list - 1
  | otherwise = pred <$> index

boundLinstIndex :: Index -> [a] -> Index
boundLinstIndex index list
  | isNothing index && (not . null) list = Just 0
  | maybe False (<= length list - 1) index = index
  | maybe False (> length list - 1) index = Just $ length list - 1
  | otherwise = Nothing
