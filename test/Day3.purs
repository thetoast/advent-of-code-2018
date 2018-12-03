module Test.Day3 where

import Prelude
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Maybe (isJust)
import Data.String.Regex (match)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')

import Day3 (Claim, Point, claimRegex, parseClaim, getClaimPoints, solve1, solve2)

claimStr1 = "#1 @ 1,3: 4x4" :: String
claimStr2 = "#2 @ 3,1: 4x4" :: String
claimStr3 = "#3 @ 5,5: 2x2" :: String
claim1 = { id: 1, point: { x: 1, y: 3 }, size: { width: 4, height: 4 } } :: Claim
claim2 = { id: 2, point: { x: 3, y: 1 }, size: { width: 4, height: 4 } } :: Claim
claim3 = { id: 3, point: { x: 5, y: 5 }, size: { width: 2, height: 2 } } :: Claim
claim1Points = [
    { x: 1, y: 3 },
    { x: 2, y: 3 },
    { x: 3, y: 3 },
    { x: 4, y: 3 },
    { x: 1, y: 4 },
    { x: 2, y: 4 },
    { x: 3, y: 4 },
    { x: 4, y: 4 },
    { x: 1, y: 5 },
    { x: 2, y: 5 },
    { x: 3, y: 5 },
    { x: 4, y: 5 },
    { x: 1, y: 6 },
    { x: 2, y: 6 },
    { x: 3, y: 6 },
    { x: 4, y: 6 }
] :: Array Point
claim2Points = [
    { x: 3, y: 1 },
    { x: 4, y: 1 },
    { x: 5, y: 1 },
    { x: 6, y: 1 },
    { x: 3, y: 2 },
    { x: 4, y: 2 },
    { x: 5, y: 2 },
    { x: 6, y: 2 },
    { x: 3, y: 3 },
    { x: 4, y: 3 },
    { x: 5, y: 3 },
    { x: 6, y: 3 },
    { x: 3, y: 4 },
    { x: 4, y: 4 },
    { x: 5, y: 4 },
    { x: 6, y: 4 }
] :: Array Point

main :: Effect Unit
main = do
    case claimRegex of
        Right pattern -> do
           assert' "Regex must work" $ isJust $ match pattern claimStr1
           assert' "Should decode claim 1" $ parseClaim pattern claimStr1 == claim1
           assert' "Should decode claim 2" $ parseClaim pattern claimStr2 == claim2
           assert' "Should decode claim 3" $ parseClaim pattern claimStr3 == claim3
        Left error -> assert' ("Regex must be valid: " <> error) false

    assert' "Points should be correct" $ getClaimPoints claim1 == claim1Points
    assert' "Points should be correct" $ getClaimPoints claim2 == claim2Points

    assert' "Should be 4" $ solve1 (claim1 : claim2 : claim3 : []) == 4
    log $ show $ solve2 (claim1 : claim2 : claim3 : [])
