module State.Cursor where

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
