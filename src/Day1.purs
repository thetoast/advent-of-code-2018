module Day1 where

import Prelude
import Data.Int as Int
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String (Pattern(..))
import Effect (Effect)
import Effect.Console (log)

type Op = Int -> Int

toOp :: String -> Op
toOp input = do
    let { before, after } = String.splitAt 1 input
    let afterInt = Int.fromString after
    case afterInt of
        Nothing -> (+) 0
        Just x -> case before of
            "-" -> flip (-) $ x
            _ -> (+) x

foldOps :: Array Op -> Int
foldOps = foldl (\acc op -> op acc) 0

solve :: String -> Int
solve input = foldOps $ toOp <$> String.split (Pattern "\n") input

day1 :: String -> Effect String
day1 input = do
    log "Day 1 started"
    pure $ show $ solve input
