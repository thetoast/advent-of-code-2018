module Test.Day1 where

import Prelude
import Effect (Effect)
import Test.Assert (assert')

import Day1 (toOp, foldOps, solve)

main :: Effect Unit
main = pure unit
--    let op1 = toOp "-1"
--    assert' "Should be -1" $ op1 1 == 0
--
--    let op2 = toOp "+1"
--    assert' "Should be 1" $ op2 1 == 2
--
--    let op3 = toOp "-2"
--    assert' "Should be -1" $ op3 1 == -1
--
--    let op4 = toOp "+2"
--    assert' "Should be +3" $ op4 1 == 3
--
--    let ops1 = [ toOp "-1", toOp "+1" ]
--    assert' "Should be 0" $ foldOps ops1 == 0
--
--    let ops2 = [ toOp "-1", toOp "-1" ]
--    assert' "Should be 0" $ foldOps ops2 == -2
--
--    let prob1 = """
---1
---1
---1
--"""
--    --assert' "Should be -3" $ solve prob1 == -3
