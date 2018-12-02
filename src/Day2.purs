module Day2 where

import Prelude
import Data.Array as Array
import Data.Foldable (foldl)
import Data.List as List
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String (Pattern(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)

type LetterDiff = Tuple String String
type WordDiff = Tuple (Tuple String String) (Array LetterDiff)

letters :: String -> Array String
letters = String.split (Pattern "")

words' :: String -> Array String
words' = String.split (Pattern "\n")

hasN :: Int -> String -> Boolean
hasN n input = do
    let res = foldl updateCount Map.empty $ letters input
    not List.null $ List.filter (eq n) $ Map.values res
    where
        updateCount = \acc cur -> Map.alter updateLetterCount cur acc
        updateLetterCount cur = case cur of
            Just x -> Just $ x + 1
            Nothing -> Just 1

getSum :: String -> Int
getSum input = do
    let words = List.fromFoldable $ words' input
    let twos = List.length $ List.filter (hasN 2) words
    let threes = List.length $ List.filter (hasN 3) words
    twos * threes

getValidBoxes :: String -> Array String
getValidBoxes input = do
    let words = words' input
    let twos = Array.filter (hasN 2) words
    let threes = Array.filter (hasN 3) words
    Array.concat $ [twos, threes]

diffWords :: String -> String -> WordDiff
diffWords s1 s2 =
    Tuple (Tuple s1 s2) (Array.filter (\(Tuple t1 t2) -> (not (eq t1 t2))) (Array.zip (letters s1) (letters s2)))

collectDiffsOfN :: Int -> List String -> List WordDiff
collectDiffsOfN n words = do
    case List.uncons words of
        Just { head, tail } -> do
            let diffs = diffWords head <$> tail
            let lenN = List.filter lengthN diffs
            case lenN of
                Nil -> collectDiffsOfN n tail
                _ -> lenN
        Nothing -> Nil
    where
        lengthN diff = eq n (Array.length (snd diff))

part1 :: String -> Effect String
part1 input = do
    log "Day2 part 1 started"
    pure $ show $ getSum input

part2 :: String -> Effect String
part2 input = do
    log "Day2 part 2 started"
    pure $ show $ collectDiffsOfN 1 $ List.fromFoldable $ getValidBoxes input
