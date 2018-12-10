module Test.Day5 where

import Prelude

import Data.Maybe (Maybe(..))
import Day5 (Polymer, condense, condenseR, makePolymer, solve1)
import Effect (Effect)
import Test.Assert (assert')

round0Str = "dabAcCaCBAcCcaDA" :: String
round0 = makePolymer "dabAcCaCBAcCcaDA" :: Polymer
round1 = makePolymer "dabAaCBAcCcaDA" :: Polymer
round2 = makePolymer "dabCBAcCcaDA" :: Polymer
round3 = makePolymer "dabCBAcaDA" :: Polymer

main :: Effect Unit
main = do
    assert' "round0 should become round1" $ condense Nothing round0 == round1
    assert' "round1 should become round2" $ condense Nothing round1 == round2
    assert' "round2 should become round3" $ condense Nothing round2 == round3
    assert' "round3 should not change" $ condense Nothing round3 == round3

    assert' "round0 should become round3" $ condenseR round0 == round3
    assert' "Part 1 works" $ solve1 round0Str == Just 10
