module Day5 where

import Prelude

import Data.Array (concat, cons, index, length, snoc, uncons) as Array
import Data.Maybe (Maybe(..))
import Data.String (toUpper) as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Util (splitChars, splitLines) as Util

type Polymer = Maybe (Array String)
type PolymerUnit = String
type PolymerUnitPair = Tuple PolymerUnit PolymerUnit
type PolymerUnitExtraction = Maybe {
    pair :: PolymerUnitPair,
    rest :: Polymer
}

-- | Creates a polymer from a string
makePolymer :: String -> Polymer
makePolymer input = Util.splitChars <$> Array.index (Util.splitLines input) 0

-- | Gets the length of a polymer
plength :: Polymer -> Maybe Int
plength = map Array.length

-- | Extracts a PolymerUnitPair and also returns the remainder
extract :: Polymer -> PolymerUnitExtraction
extract polymer = case plength polymer of
    Nothing -> Nothing
    Just 1 -> Nothing
    Just 2 -> Nothing
    _ -> do
        p1 <- Array.uncons =<< polymer
        p2 <- Array.uncons p1.tail
        pure $ { pair: Tuple p1.head p2.head, rest: pure p2.tail }

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

-- | "Condenses" a polymer according to rules
condense :: Polymer -> Polymer -> Polymer
condense acc polymer = case extract polymer of
    Nothing -> join acc polymer
    Just { pair: (Tuple u1 u2), rest } -> case canDrop u1 u2 of
        true -> join acc rest
        false -> condense (snoc acc u1) ((Array.cons u2) <$> rest)

-- | Recursively condenses a polymer until it's fully condensed
condenseR :: Polymer -> Polymer
condenseR polymer = do
    let res = condense Nothing polymer
    if res == polymer then res else condenseR res

solve1 :: String -> Maybe Int
solve1 = plength <<< condenseR <<< makePolymer

part1 :: String -> Effect String
part1 = pure <<< show <<< solve1

part2 :: String -> Effect String
part2 = pure
