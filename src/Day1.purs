module Day1 where

import Prelude
import Effect (Effect)
import Effect.Console (log)

day1 :: String -> Effect String
day1 input = do
    log "Day 1 started"
    pure $ "Hey! " <> input <> " is the result for Day 1!"
