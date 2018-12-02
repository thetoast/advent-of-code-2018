module Test.Day2 where

import Prelude
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.Assert (assert')

import Day2 (hasN, diffWords)

main :: Effect Unit
main = do
    assert' "should have 2" $ hasN 2 "aba"
    assert' "should not have 2" $ not $ hasN 2 "ab"

    assert' "should have 3" $ hasN 3 "ababa"
    assert' "should not have 3" $ not $ hasN 3 "abab"

    let w1 = "aaa"
    let w2 = "aba"
    let w3 = "abb"
    assert' "should be ('a' 'b')" $ diffWords w1 w2 == Tuple (Tuple w1 w2) [Tuple "a" "b"]
    assert' "should be ('a' 'b'),('a' 'b')" $ diffWords w1 w3 == Tuple (Tuple w1 w3) [Tuple "a" "b", Tuple "a" "b"]

