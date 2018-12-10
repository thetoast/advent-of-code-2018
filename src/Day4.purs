module Day4 where

import Prelude
import Data.Array ((..))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, index, (!!), length)
import Data.DateTime (DateTime(..), diff, Time(..), exactDate, time)
import Data.Either (hush)
import Data.Enum (toEnum, fromEnum)
import Data.Foldable (foldl)
import Data.Int as Int
import Data.List as List
import Data.List (List(..))
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, class Newtype)
import Data.String.Regex (Regex, regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.Time (minute)
import Data.Time.Duration (Minutes(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Traversable (traverse)
import Effect (Effect)
import Util (splitLines)

newtype Guard = Guard Int
derive instance eqGuard :: Eq Guard
derive instance ordGuard :: Ord Guard
derive instance newtypeGuard :: Newtype Guard _
instance showGuard :: Show Guard where
    show (Guard n) = "Guard " <> (show n)

data GuardEvent
    = Start DateTime Guard
    | Sleep DateTime
    | Wake DateTime
derive instance eqGuardEvent :: Eq GuardEvent
instance showGuardEvent :: Show GuardEvent where
    show (Start t g) = "Start - " <> show t <> " - " <> show g
    show (Sleep t) = "Sleep - " <> show t
    show (Wake t) = "Wake - " <> show t
instance ordGuardEvent :: Ord GuardEvent where
    compare (Start t1 _) (Start t2 _) = compare t1 t2
    compare (Start t1 _) (Sleep t2) = compare t1 t2
    compare (Start t1 _) (Wake t2) = compare t1 t2
    compare (Sleep t1) (Start t2 _) = compare t1 t2
    compare (Sleep t1) (Sleep t2) = compare t1 t2
    compare (Sleep t1) (Wake t2) = compare t1 t2
    compare (Wake t1) (Start t2 _) = compare t1 t2
    compare (Wake t1) (Sleep t2) = compare t1 t2
    compare (Wake t1) (Wake t2) = compare t1 t2

type Solve1Data = {
    currentGuard :: Maybe Guard,
    currentSleep :: Maybe DateTime,
    guardTimes :: Map Guard Minutes,
    guardRanges :: Map Guard (List (Tuple DateTime DateTime))
}

-- "[1518-11-01 00:00]"
dateRegex :: Maybe Regex
dateRegex = hush $ regex """\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\]""" noFlags

--[1518-11-01 00:00] Guard #10 begins shift
--[1518-11-01 00:05] falls asleep
--[1518-11-01 00:25] wakes up
logRegex :: Maybe Regex
logRegex = hush $ regex """(\[.*\]) (Guard #(\d+) begins shift|.*)""" noFlags

createDate :: NonEmptyArray (Maybe String) -> Maybe DateTime
createDate matches = do
    year <- toEnum =<< getMatchAsInt 1
    month <- toEnum =<< getMatchAsInt 2
    day <- toEnum =<< getMatchAsInt 3
    hour <- toEnum =<< getMatchAsInt 4
    minute <- toEnum =<< getMatchAsInt 5
    second <- toEnum 0
    millis <- toEnum 0
    date <- exactDate year month day
    pure $ DateTime date (Time hour minute second millis)
    where getMatchAsInt = Int.fromString <=< join <<< index matches

parseDate :: String -> Maybe DateTime
parseDate dateStr = case dateRegex of
    Just pattern -> case match pattern dateStr of
        Just matches -> createDate matches
        Nothing -> Nothing
    Nothing -> Nothing

createEvent :: NonEmptyArray (Maybe String) -> Maybe GuardEvent
createEvent matches = do
    date <- join $ parseDate <$> (join $ matches !! 1)
    evt <- matches !! 2
    case evt of
        Just "falls asleep" -> Just $ Sleep date
        Just "wakes up" -> Just $ Wake date
        Just x -> case join $ matches !! 3 of
            Just num -> case Int.fromString num of
                Just n -> Just $ Start date (Guard n)
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing

parseLog :: String -> Maybe GuardEvent
parseLog logStr = case logRegex of
    Just pattern -> case match pattern logStr of
        Just matches -> createEvent matches
        Nothing -> Nothing
    Nothing -> Nothing

parseEvents :: String -> Maybe (Array GuardEvent)
parseEvents input = traverse parseLog $ splitLines input

updateSleepTime :: Solve1Data -> DateTime -> Solve1Data
updateSleepTime acc t2 =
    case acc.currentGuard of
        Just g -> case acc.currentSleep of
            Just t1 -> do
                let guardTimes = Map.alter (updateTime t1) g acc.guardTimes
                let guardRanges = Map.alter (updateRange t1) g acc.guardRanges
                acc { guardTimes = guardTimes, guardRanges = guardRanges}
            Nothing -> acc
        Nothing -> acc
    where
        updateTime t1 v = case v of
            Nothing -> Just (diff t2 t1)
            Just (Minutes min) -> case (diff t2 t1) of
                Minutes dMin -> Just (Minutes (min + dMin))
        updateRange t1 v = case v of
            Nothing -> Just $ List.singleton (Tuple t1 t2)
            Just list -> Just $ Cons (Tuple t1 t2) list

handleEvent :: Solve1Data -> GuardEvent -> Solve1Data
handleEvent acc evt = do
    case evt of
        Start t g -> acc { currentGuard = Just g }
        Sleep t -> case acc.currentGuard of
            Just _ -> acc { currentSleep = Just t }
            Nothing -> acc
        Wake t -> case acc.currentGuard of
            Just _ -> do
               updateSleepTime acc t
            Nothing -> acc

generateData :: String -> Maybe Solve1Data
generateData input = do
    sorted <- Array.sort <$> parseEvents input
    let acc = { currentGuard: Nothing, currentSleep: Nothing, guardTimes: Map.empty, guardRanges: Map.empty }
    pure $ foldl handleEvent acc sorted

findSleepiestGuard :: String -> Maybe (Tuple Guard (List (Tuple DateTime DateTime)))
findSleepiestGuard input = do
    res <- generateData input
    let pairs = Map.toUnfoldableUnordered res.guardTimes
    guard <- fst <$> (Array.head $ Array.sortBy orderPairs pairs)
    list <- Map.lookup guard res.guardRanges
    Just $ Tuple guard list
    where
        orderPairs (Tuple (Guard g1) (Minutes m1)) (Tuple (Guard g2) (Minutes m2)) =
            compare m2 m1 -- reverse

findSleepiestMinute :: List (Tuple DateTime DateTime) -> Maybe (Tuple Int Int)
findSleepiestMinute list = do
    let minutes = Array.concat $ Array.fromFoldable $ toMinutesArr <$> list
    let groups = Array.group' minutes
    let sorted = Array.sortBy longestArr groups
    biggest <- Array.head sorted
    biggestValue <- biggest !! 0
    Just $ Tuple biggestValue (length biggest)
    where
        getMinutes = fromEnum <<< minute <<< time
        toMinutesArr (Tuple t1 t2) = (getMinutes t1)..(getMinutes t2)
        longestArr a1 a2 = compare (length a2) (length a1) -- reverse

solve1 :: String -> Maybe Int
solve1 input = do
    sleepiest <- findSleepiestGuard input
    let guard = fst sleepiest
    let times = snd sleepiest
    sleepiestMinute <- findSleepiestMinute times
    Just $ (mul $ unwrap guard) $ (fst $ sleepiestMinute)

solve2 :: String -> Maybe Int
solve2 input = do
    res <- generateData input
    sleepiestByGuard <- traverse findSleepiestMinute res.guardRanges
    let pairs = Map.toUnfoldableUnordered sleepiestByGuard
    let sorted = Array.sortBy mostTimes pairs
    choice <- Array.head sorted
    let guard = fst choice
    let minute = fst $ snd choice
    Just $ (unwrap guard) * minute
    where
        mostTimes (Tuple _ (Tuple _ len1)) (Tuple _ (Tuple _ len2)) =
            compare len2 len1 -- reverse

part1 :: String -> Effect String
part1 = pure <<< show <<< solve1

part2 :: String -> Effect String
part2 = pure <<< show <<< solve2
