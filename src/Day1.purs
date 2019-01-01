module Day1 where

import Prelude

import Control.Monad.Except (throwError)
import Control.Monad.RWS (ask)
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Util (MainProgram, MainState, Program, log')

type Op = Int -> Int
type Part2Result = { sum :: Int, prev :: Set Int, result :: Maybe Int}

toOp :: String -> Program MainState Op
toOp input = do
    let { before, after } = String.splitAt 1 input
    let afterInt = Int.fromString after
    case afterInt of
       Nothing -> throwError ["invalid int: " <> after]
       Just x -> case before of
         "-" -> pure $ flip (-) $ x
         "+" -> pure $ (+) x
         a -> throwError ["invalid operation: " <> a]

foldOps :: Array Op -> Int
foldOps = foldl (\acc op -> op acc) 0

solve :: String -> Program MainState Int
solve input = foldOps <$> (traverse toOp $ String.split (Pattern "\n") input)

opAndCheck :: Part2Result -> Op -> Part2Result
opAndCheck acc op = case acc.result of
    Just _ -> acc
    Nothing -> do
       let sum = op acc.sum
       case Set.member sum acc.prev of
            true -> acc { result = Just sum }
            _ -> acc { sum = sum, prev = Set.insert sum acc.prev }

foldOps2 :: Part2Result -> Array Op -> Part2Result
foldOps2 acc = foldl opAndCheck acc

solve2 :: Part2Result -> Array Op -> Int
solve2 acc ops = do
    let res = foldOps2 acc ops
    case res.result of
         Just x -> x
         Nothing -> solve2 res ops

part1 :: MainProgram
part1 = do
  input <- ask
  log' "Day 1 Part 1 started"
  show <$> solve input

part2 :: MainProgram
part2 = do
  input <- ask
  log' "Day 1 Part 2 started"
  ops <- traverse toOp $ String.split (Pattern "\n") input
  pure $ show $ solve2 { sum: 0, prev: Set.singleton 0, result: Nothing} ops
