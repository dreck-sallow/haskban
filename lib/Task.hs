module Task (Task) where

data Task = Task
  { taskId :: Int,
    title :: String,
    description :: String,
    endDate :: String
  }
  deriving (Show)


