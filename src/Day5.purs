module Day5 where

import Prelude

import Data.Array ((!!))
import Data.Array (concat, cons, drop, index, length, snoc, take, takeEnd, uncons) as Array
import Data.Maybe (Maybe(..))
import Data.String (toUpper) as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Util (splitChars, splitLines) as Util

type Polymer = Maybe (Array String)
type PolymerUnit = String
type PolymerUnitPair = Tuple PolymerUnit PolymerUnit

-- | Creates a polymer from a string
makePolymer :: String -> Polymer
makePolymer input = Util.splitChars <$> Array.index (Util.splitLines input) 0

-- | Gets the length of a polymer
plength :: Polymer -> Maybe Int
plength = map Array.length

-- | Extracts a PolymerUnitPair and also returns the remainder
extract :: Int -> Polymer -> Maybe PolymerUnitPair
extract pos polymer = case sub <$> plength polymer <*> Just pos of
    Nothing -> Nothing
    Just 0 -> Nothing
    Just 1 -> Nothing
    Just 2 -> Nothing
    _ -> case polymer of
        Nothing -> Nothing
        Just arr -> do
            p1 <- arr !! pos
            p2 <- arr !! (pos + 1)
            pure $ Tuple p1 p2

-- | Determines if two polymer units can be combined
canDrop :: PolymerUnit -> PolymerUnit -> Boolean
canDrop u1 u2 =
    (not (u1 == u2))
    && (((String.toUpper u1) == u2) || ((String.toUpper u2) == u1))

-- | Joins to polymers
join :: Polymer -> Polymer -> Polymer
join Nothing Nothing = Nothing
join Nothing (Just list) = Just list
join (Just list) Nothing = Just list
join (Just list1) (Just list2) = pure $ Array.concat [list1, list2]

-- | Appends a PolymerUnit to the end of a Polymer
snoc :: Polymer -> PolymerUnit -> Polymer
snoc Nothing u = pure [u]
snoc (Just p) u = pure $ (Array.snoc p u)

deleteAt :: Int -> Int -> Polymer -> Polymer
deleteAt idx1 idx2 p = do
    let first = Array.take idx1 <$> p
    lastAmt <- sub <$> plength p <*> Just (idx2+1)
    let last = Array.takeEnd lastAmt <$> p
    join first last

-- | "Condenses" a polymer according to rules
condense :: Int -> Polymer -> Polymer
condense pos polymer = case extract pos polymer of
    Nothing -> polymer
    Just (Tuple u1 u2) -> case canDrop u1 u2 of
        true -> deleteAt pos (pos+1) polymer
        false -> condense (pos+1) polymer

-- | Recursively condenses a polymer until it's fully condensed
condenseR :: Polymer -> Polymer
condenseR polymer = do
    let res = condense 0 polymer
    if (plength res) == (plength polymer) then res else condenseR res

solve1 :: String -> Maybe Int
solve1 = plength <<< condenseR <<< makePolymer

part1 :: String -> Effect String
part1 = pure <<< show <<< solve1

part2 :: String -> Effect String
part2 = pure
