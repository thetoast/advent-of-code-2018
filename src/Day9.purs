module Day9 where

import Prelude

import CircularList (insertRight, popCurrent, shiftLeftN, shiftRight, singleton) as CL
import Control.Monad.State (get)
import Data.Array ((..))
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map (alter, empty) as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Util (MainProgram, Program, log', maxValue, runSubprogram)

type Player = Int
type Score = BigInt
type Day9State = Map Player Score
type Day9Program = Program Day9State

initialState :: Day9State
initialState = Map.empty

solve1 :: Day9Program String
solve1 = do
  log' "Part 1 started"
  scores <- get
  let clStart = CL.singleton 0
  let acc = { cl: clStart, scores }
  let highestMarble = 71240
  let final = foldl insertMarble acc (1 .. highestMarble)

  pure $ show $ maxValue final.scores

  where
    numPlayers = 478
    insertMarble acc@{ cl, scores } x
      | (x `mod` 23) == 0 = scoreMarble acc x
      | otherwise = acc { cl = CL.insertRight x $ CL.shiftRight cl }
    scoreMarble { cl, scores } x =
      let (Tuple popped cl') = unsafePartial $ fromJust $ CL.popCurrent $ CL.shiftLeftN 7 cl
          player = x `mod` numPlayers
          scores' = Map.alter (updateScore (x+popped)) player scores
       in { cl: cl', scores: scores' }
    updateScore amount Nothing = Just (BigInt.fromInt amount)
    updateScore amount (Just prev) = Just (prev + (BigInt.fromInt amount))

part1 :: MainProgram
part1 = runSubprogram solve1 initialState

solve2 :: Day9Program String
solve2 = do
  log' "Part 2 started"
  scores <- get
  let clStart = CL.singleton 0
  let acc = { cl: clStart, scores }
  let highestMarble = 7124000
  let final = foldl insertMarble acc (1 .. highestMarble)

  pure $ show $ maxValue final.scores

  where
    numPlayers = 478
    insertMarble acc@{ cl, scores } x
      | (x `mod` 23) == 0 = scoreMarble acc x
      | otherwise = acc { cl = CL.insertRight x $ CL.shiftRight cl }
    scoreMarble { cl, scores } x =
      let (Tuple popped cl') = unsafePartial $ fromJust $ CL.popCurrent $ CL.shiftLeftN 7 cl
          player = x `mod` numPlayers
          scores' = Map.alter (updateScore (x+popped)) player scores
       in { cl: cl', scores: scores' }
    updateScore amount Nothing = Just (BigInt.fromInt amount)
    updateScore amount (Just prev) = Just (prev + (BigInt.fromInt amount))

part2 :: MainProgram
part2 = runSubprogram solve2 initialState
