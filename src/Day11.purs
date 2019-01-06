module Day11 where

import Prelude

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Array (modifyAt, updateAt, (!!), (..))
import Data.Foldable (foldl)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (sequence, traverse, traverse_)
import Partial.Unsafe (unsafePartial)
import Util (MainProgram, Point, Program, log', runSubprogram)

type Serial = Int
type PowerCell = Int
type PowerGrid = Array (Array PowerCell)
type Day11State = {
  grid :: PowerGrid,
  largestGroup :: Maybe Point,
  largestSize :: Maybe Int,
  largestValue :: Maybe Int
}
type Day11Program = Program Day11State

sumAt :: Serial -> PowerGrid -> Point -> PowerGrid
sumAt serial grid point@{ x: 0, y: 0 } =
  insertPowerLevel point (calculatePowerLevel point serial) grid
sumAt serial grid point@{ x, y: 0 } =
  let left = unsafePartial $ fromJust $ (grid !! (x-1) >>= (_ !! 0))
   in insertPowerLevel point (left + (calculatePowerLevel point serial)) grid
sumAt serial grid point@{ x: 0, y } =
  let above = unsafePartial $ fromJust $ (grid !! 0 >>= (_ !! (y-1)))
   in insertPowerLevel point (above + (calculatePowerLevel point serial)) grid
sumAt serial grid point@{ x, y } =
  let left = unsafePartial $ fromJust $ (grid !! (x-1) >>= (_ !! y))
      above = unsafePartial $ fromJust $ (grid !! x >>= (_ !! (y-1)))
      diagonal = unsafePartial $ fromJust $ (grid !! (x-1) >>= (_ !! (y-1)))
      delta = above + left - diagonal
   in insertPowerLevel point (delta + (calculatePowerLevel point serial)) grid

initialState :: Serial -> Day11State
initialState serial =
  let points = join $ traverse { x: _, y: _ } (0..299) <$> (0..299)
      initialGrid = (\x -> ((\y -> 0) <$> (0..299))) <$> (0..299)
      grid = foldl (sumAt serial) initialGrid points
   in { grid, largestGroup: Nothing, largestSize: Nothing, largestValue: Nothing }

--Find the fuel cell's rack ID, which is its X coordinate plus 10.
--Begin with a power level of the rack ID times the Y coordinate.
--Increase the power level by the value of the grid serial number (your puzzle input).
--Set the power level to itself multiplied by the rack ID.
--Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
--Subtract 5 from the power level.
calculatePowerLevel :: Point -> Serial -> Int
calculatePowerLevel { x, y } serial =
  let rackId = x + 1 + 10
      step1 = rackId * (y + 1)
      step2 = step1 + serial
      step3 = step2 * rackId
      finalLevel = (step3 / 100) `mod` 10
   in finalLevel - 5

insertPowerLevel :: Point -> Int -> PowerGrid -> PowerGrid
insertPowerLevel { x, y } cell grid =
  let updateY col = unsafePartial $ fromJust $ updateAt y cell col
      updateCell = unsafePartial $ fromJust $ modifyAt x updateY grid
   in updateCell

getGroupPowerLevel :: Point -> Int -> PowerGrid -> Int
getGroupPowerLevel {x, y} size grid =
  let rightX  = x + size - 1
      bottomY = y + size - 1
      lookup x' y' = unsafePartial $ fromJust $ ((grid !! x') >>= (_ !! y'))
      target   = lookup rightX bottomY
      calculate 0 0 = target
      calculate 0 _ = target - (lookup rightX (y-1))
      calculate _ 0 = target - (lookup (x-1) bottomY)
      calculate _ _ = target - (lookup (x-1) bottomY) -
                      (lookup rightX (y-1)) + lookup (x-1) (y-1)
   in calculate x y

getAndUpdateMaxGroupLevel :: Point -> Int -> Day11Program Unit
getAndUpdateMaxGroupLevel point size = do
  state@{ largestGroup, largestValue, grid } <- get
  let value = getGroupPowerLevel point size grid
  case largestValue of
    Nothing -> put state {
      largestGroup = (Just point),
      largestSize = (Just size),
      largestValue = (Just value)
    }
    Just value' -> if value > value'
      then put state {
        largestGroup = (Just point),
        largestSize = (Just size),
        largestValue = (Just value)
      }
      else pure unit

findLargestOfSize :: Int -> Day11Program Unit
findLargestOfSize size =  do
  serial <- unsafePartial $ fromJust <$> Int.fromString <$> ask
  let getLevel point = getAndUpdateMaxGroupLevel point size
      maxIndex = 299 - size + 1
  _ <- sequence $ getLevel <$> (traverse { x: _, y: _ } (0..maxIndex) =<< (0..maxIndex))
  pure unit

solve1 :: Day11Program String
solve1 = do
  log' "Part 1 started"
  findLargestOfSize 3
  { largestGroup } <- get
  case largestGroup of
    Nothing -> throwError ["did not find largest group"]
    Just group -> pure $ (show (group.x+1)) <> "," <> (show (group.y+1))

part1 :: MainProgram
part1 = do
  serial <- unsafePartial $ fromJust <$> Int.fromString <$> ask
  runSubprogram solve1 (initialState serial)

solve2 :: Day11Program String
solve2 = do
  log' "Part 2 started"
  traverse_ findLargestOfSize (1..300)
  { largestGroup, largestSize, largestValue } <- get
  case largestGroup of
    Nothing -> throwError ["did not find largest group"]
    Just group -> case largestSize of
                       Nothing -> throwError ["did not find largest size"]
                       Just size -> case largestValue of
                                         Nothing -> throwError ["did not find largest value"]
                                         Just value -> pure $
                                           (show (group.x+1)) <> "," <>
                                             (show (group.y+1)) <> "," <>
                                               (show size) <> ": " <> (show value)

part2 :: MainProgram
part2 = do
  serial <- unsafePartial $ fromJust <$> Int.fromString <$> ask
  runSubprogram solve2 (initialState serial)
