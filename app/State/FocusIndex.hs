module State.FocusIndex where

import Data.Bool (bool)
import Data.Maybe (isNothing)

type Index = Maybe Int

data Cursor = Empty | GroupI Int | TaskI Int Int

instance Show Cursor where
  show Empty = "()"
  show (GroupI idx) = "(" ++ show idx ++ ")"
  show (TaskI groupIdx taskIdx) = "(" ++ show groupIdx ++ ", " ++ show taskIdx ++ ")"

-- method for cursor datatype
isEmpty :: Cursor -> Bool
isEmpty Empty = True
isEmpty _ = False

isGroupI :: Cursor -> Bool
isGroupI (GroupI _) = True
isGroupI _ = False

isTaskI :: Cursor -> Bool
isTaskI (TaskI _ _) = True
isTaskI _ = False

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

-- TODO: rename function becasue when the index is Nothing, I append to Just 0 (where we can only check if the index is bound to list)
boundLinstIndex :: Index -> [a] -> Index
boundLinstIndex index list
  | isNothing index && (not . null) list = Just 0
  | maybe False (<= length list - 1) index = index
  | maybe False (> length list - 1) index = Just $ bool 0 (length list - 1) ((length list - 1) >= 0)
  | otherwise = Nothing
