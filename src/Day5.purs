module Day5 where

import Prelude

import Data.Array ((!!))
import Data.Array (concat, head, index, length, snoc, sort, take, takeEnd) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (toUpper) as String
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global, ignoreCase)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
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
pjoin :: Polymer -> Polymer -> Polymer
pjoin Nothing Nothing = Nothing
pjoin Nothing (Just list) = Just list
pjoin (Just list) Nothing = Just list
pjoin (Just list1) (Just list2) = pure $ Array.concat [list1, list2]

-- | Appends a PolymerUnit to the end of a Polymer
snoc :: Polymer -> PolymerUnit -> Polymer
snoc Nothing u = pure [u]
snoc (Just p) u = pure $ (Array.snoc p u)

deleteAt :: Int -> Int -> Polymer -> Polymer
deleteAt idx1 idx2 p = do
    let first = Array.take idx1 <$> p
    lastAmt <- sub <$> plength p <*> Just (idx2+1)
    let last = Array.takeEnd lastAmt <$> p
    pjoin first last

-- | "Condenses" a polymer according to rules
condense :: Int -> Polymer -> Polymer
condense pos polymer = case extract pos polymer of
    Nothing -> polymer
    Just (Tuple u1 u2) -> case canDrop u1 u2 of
        true -> condense (max (pos-1) 0) (deleteAt pos (pos+1) polymer)
        false -> condense (pos+1) polymer

solve1 :: String -> Maybe Int
solve1 = plength <<< condense 0 <<< makePolymer

dropAll :: String -> String -> String
dropAll letter string = do
    case regex (letter<>"+") (global <> ignoreCase) of
        Right pattern -> replace pattern "" string
        Left _ -> string

part1 :: String -> Effect String
part1 = pure <<< show <<< solve1

letters :: Array String
letters = [
    "a", "b", "c", "d", "e", "f", "g", "h",
    "i", "j", "k", "l", "m", "n", "o", "p", "q",
    "r", "s", "t", "u", "v", "w", "x", "y", "z"
]

solve2 :: String -> String -> Effect (Maybe Int)
solve2 letter input = do
    log $ "Solving for " <> letter
    let res = solve1 $ dropAll letter input
    log $ show res
    pure res



part2 :: String -> Effect String
part2 input = do
    log "Calculating stage1s"
    res <- traverse (flip solve2 input) letters
    pure $ show $ Array.head <$> Array.sort <$> sequence res
