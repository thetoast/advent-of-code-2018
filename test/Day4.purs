module Test.Day4 where

import Prelude
import Data.DateTime (DateTime(..), Time(..), exactDate)
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), isJust, fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert')
import Day4 (dateRegex, logRegex, parseDate, parseLog, GuardEvent(..), Guard(..), parseEvents)

makeDate :: Int -> Int -> Int -> Int -> Int -> Maybe DateTime
makeDate year mon day hr min = DateTime <$>
    (join $ exactDate <$> toEnum year <*> toEnum mon <*> toEnum day)
    <*> (Time <$> toEnum hr <*> toEnum min <*> toEnum 0 <*> toEnum 0)

dateStr1 = "[1518-11-01 00:00]" :: String
dateTime1 = makeDate 1518 11 1 0 0 :: Maybe DateTime

logStr1 = "[1518-11-01 00:00] Guard #10 begins shift" :: String
logStr2 = "[1518-11-01 00:05] falls asleep" :: String
logStr3 = "[1518-11-01 00:25] wakes up" :: String
logEvent1 = Start (unsafePartial $ fromJust $ makeDate 1518 11 1 0 0) (Guard 10) :: GuardEvent
logEvent2 = Sleep (unsafePartial $ fromJust $ makeDate 1518 11 1 0 5) :: GuardEvent
logEvent3 = Wake (unsafePartial $ fromJust $ makeDate 1518 11 1 0 25) :: GuardEvent

main :: Effect Unit
main = do
    assert' "Date regex is valid" $ isJust dateRegex
    assert' "Log regex is valid" $ isJust logRegex
    assert' ("Date should be " <> dateStr1) $ parseDate dateStr1 == dateTime1
    assert' "Log 1 should be parsed" $ parseLog logStr1 == Just logEvent1
    assert' "Log 2 should be parsed" $ parseLog logStr2 == Just logEvent2
    assert' "Log 3 should be parsed" $ parseLog logStr3 == Just logEvent3
    assert' "Logs 1-3 should be parsed" $
        parseEvents (logStr1 <> "\n" <> logStr2 <> "\n" <> logStr3) ==
        Just [logEvent1, logEvent2, logEvent3]

