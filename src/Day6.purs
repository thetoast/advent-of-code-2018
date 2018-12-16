module Day6 where

import Prelude

import Data.Array ((..))
import Data.Array (concat, filter, head, length) as Array
import Data.Array.NonEmpty ((!!))
import Data.Either (hush)
import Data.Foldable (sum)
import Data.Int (fromString) as Int
import Data.Map (alter, empty, toUnfoldable) as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (abs)
import Data.Set (Set)
import Data.Set (empty, insert, member) as Set
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (foldl, sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple (snd) as Tuple
import Effect (Effect)
import Util (Area, Point)
import Util (makePoint, maxValue', splitLines) as Util

newtype Distance = Distance (Tuple Point Int)
derive instance eqDistance :: Eq Distance
derive instance newtypeDistance :: Newtype Distance _
instance ordDistance :: Ord Distance where
  compare (Distance (Tuple _ d1)) (Distance (Tuple _ d2)) = compare d1 d2

makePoint :: Regex → String → Maybe Point
makePoint pattern line = case match pattern line of
  Just matches -> do
     x <- join $ Int.fromString <$> (join $ (matches !! 1))
     y <- join $ Int.fromString <$> (join $ matches !! 2)
     Just { x: x, y: y }
  Nothing -> Nothing

parsePoints :: String → Maybe (Array Point)
parsePoints input = do
  pattern <- hush $ regex """(\d+), (\d+)""" noFlags
  traverse (makePoint pattern) (Util.splitLines input)

distance :: Point -> Point -> Int
distance p1 p2 = abs(p1.x - p2.x) + abs(p1.y - p2.y)

distanceTuple :: Point -> Point -> Distance
distanceTuple p1 p2 = Distance (Tuple p2 (distance p1 p2))

getPoint :: Distance -> Point
getPoint (Distance (Tuple p d)) = p

calcArea :: Array Point -> Area
calcArea points = do
  let minX = foldl min 100 $ _.x <$> points
  let minY = foldl min 100 $ _.y <$> points
  let maxX = foldl max 0 $ _.x <$> points
  let maxY = foldl max 0 $ _.y <$> points
  {
    point: { x: minX, y: minY },
    size: { width: maxX - minY + 1, height: maxY - minY + 1 }
  }

getDistances :: Point -> Array Point -> Array Distance
getDistances point = map (distanceTuple point)

getClosestPoint :: Point -> Array Point -> Maybe (Tuple Point Point)
getClosestPoint point points = do
  first <- distanceTuple point <$> Array.head points
  let distances = getDistances point points
  let shortest = foldl min first distances
  let shortestLen = Tuple.snd $ unwrap shortest
  let allShortest = Array.filter (\(Distance (Tuple _ dist)) -> dist == shortestLen) distances
  case Array.length allShortest of
    1 -> pure $ Tuple point $ getPoint $ shortest
    _ -> Nothing

allPointsInArea :: Area -> Array (Array Point)
allPointsInArea area =
  traverse Util.makePoint xs <$> ys
  where
    xs = area.point.x .. (area.point.x + area.size.width)
    ys = area.point.y .. (area.point.y + area.size.height)

findInfinite :: Array (Tuple Point Point) -> Area -> Set Point
findInfinite points area = foldl findEdge Set.empty points
  where
    minX = area.point.x
    minY = area.point.y
    maxX = area.point.x + area.size.width - 1
    maxY = area.point.y + area.size.height - 1
    findEdge acc (Tuple point closest) = if
      (point.x == minX || point.x == maxX || point.y == minY || point.y == maxY)
      then Set.insert closest acc else acc

solve1 :: String -> Maybe (Tuple Point Int)
solve1 input = do
  inputPoints <- parsePoints input
  let area = calcArea inputPoints
  let allPoints = Array.concat $ allPointsInArea area
  allClosest <- sequence $ Array.filter isJust $ (flip getClosestPoint inputPoints) <$> allPoints
  let infPoints = findInfinite allClosest area
  let pointCounts = Map.toUnfoldable $ foldl countPoints Map.empty allClosest
  let finiteCounts = Array.filter (\(Tuple k _) -> not $ Set.member k infPoints) pointCounts
  Util.maxValue' finiteCounts

  where
    countPoints acc (Tuple _ cur) = Map.alter incCount cur acc
    incCount value = pure $ case value of
      Nothing -> 1
      Just v -> v+1

solve2 :: String -> Maybe Int
solve2 input = do
  inputPoints <- parsePoints input
  let area = calcArea inputPoints
  let allPoints = Array.concat $ allPointsInArea area
  let allDistances = (flip getDistances inputPoints) <$> allPoints
  pure $ Array.length $ Array.filter (sumLt 10000) allDistances
  where sumLt i dists = (sum $ (\(Distance (Tuple _ d)) -> d) <$> dists) < i

part1 :: String -> Effect String
part1 input = pure $ show $ solve1 input

part2 :: String -> Effect String
part2 input = pure $ show $ solve2 input
