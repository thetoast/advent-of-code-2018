module Test.Day5 where

import Prelude

import Data.Maybe (Maybe(..))
import Day5 (Polymer, condense, makePolymer, dropAll, solve1)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')

round0Str = "dabAcCaCBAcCcaDA" :: String
round0 = makePolymer "dabAcCaCBAcCcaDA" :: Polymer
round1 = makePolymer "dabAaCBAcCcaDA" :: Polymer
round2 = makePolymer "dabCBAcCcaDA" :: Polymer
round3 = makePolymer "dabCBAcaDA" :: Polymer
round0StrNoA = "dbcCCBcCcD" :: String
round0StrNoB = "daAcCaCAcCcaDA" :: String
round0StrNoC = "dabAaBAaDA" :: String
round0StrNoD = "abAcCaCBAcCcaA" :: String

main :: Effect Unit
main = do
    assert' "round3 should not change" $ condense 0 round3 == round3

    assert' "round0 should become round3" $ condense 0 round0 == round3

    assert' "Part 1 works" $ solve1 round0Str == Just 10

    assert' "round0 without As should work" $ dropAll "a" round0Str == round0StrNoA
