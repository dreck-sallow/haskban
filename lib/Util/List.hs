module Util.List where

data Movement = Prev | Next

isPrevMove :: Movement -> Bool
isPrevMove Prev = True
isPrevMove _ = False

isNextMove :: Movement -> Bool
isNextMove mv = not $ isPrevMove mv

-- Compute the index

moveIndex :: Movement -> Int -> [a] -> Maybe Int
moveIndex Prev = prevIndex
moveIndex Next = nextIndex

nextIndex :: Int -> [a] -> Maybe Int
nextIndex _ [] = Nothing
nextIndex idx list
  | idx < 0 || idx >= (length list - 1) = Nothing
  | otherwise = Just (idx + 1)

prevIndex :: Int -> [a] -> Maybe Int
prevIndex _ [] = Nothing
prevIndex idx list
  | idx <= 0 || idx >= length list = Nothing -- Out of list bounds
  | otherwise = Just (idx - 1)

inListIndex :: Int -> [a] -> Bool
inListIndex _i [] = False
inListIndex i list = i == 0 && i < length list

notInListIndex :: Int -> [a] -> Bool
notInListIndex i list = not $ inListIndex i list

-- List modifiers
swapIndices :: (Int, Int) -> [a] -> [a]
swapIndices _ [] = []
swapIndices (j, k) list
  | j == k = list
  | notInListIndex j list && notInListIndex k list = []
  | otherwise = zipWith swap' [0 ..] list
  where
    -- FIXME: improve the swap function
    swap' i itm
      | i == j = list !! k
      | i == k = list !! j
      | otherwise = itm

replaceByIndex :: Int -> a -> [a] -> [a]
replaceByIndex _ _ [] = []
replaceByIndex idx itm list
  | notInListIndex idx list = list
  | otherwise =
      let (x, xs) = splitAt idx list
       in x ++ itm : tail xs

insertByIndex :: Int -> a -> [a] -> [a]
insertByIndex 0 itm list = itm : list
insertByIndex idx itm list
  | idx >= length list = list <> [itm]
  | otherwise =
      let (x, xs) = splitAt idx list
       in x ++ itm : xs

removeByIndex :: Int -> [a] -> [a]
removeByIndex _ [] = []
removeByIndex idx list
  | notInListIndex idx list = list
  | otherwise = take idx list ++ drop (idx + 1) list
