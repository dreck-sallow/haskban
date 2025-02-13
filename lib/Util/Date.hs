module Util.Date where

import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

currentTimestamp :: IO Int
currentTimestamp = do
  currentTime <- getCurrentTime
  let timestamp = utcTimeToPOSIXSeconds currentTime
  return $ round timestamp
