module Day3 where

import Prelude

import Control.Monad.RWS (ask)
import Data.Array ((..))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, (!!))
import Data.Either (Either(..))
import Data.Foldable (foldl, maximum)
import Data.Int as Int
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String.Regex (Regex, regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Util (Point, Size, Solution, Program, splitLines)

type Claim = { id :: Int, point :: Point, size :: Size }
type Fabric = Array (Array (List Claim))

emptyClaim :: Claim
emptyClaim = { id: 0, point: { x: 0, y: 0}, size: { width: 0, height: 0} }

-- "#1 @ 1,3: 4x4"
claimRegex :: Either String Regex
claimRegex = regex "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)" noFlags

parseGroups :: NonEmptyArray (Maybe String) -> Claim
parseGroups groups = {
        id: parseIndex 1,
        point: { x: parseIndex 2, y: parseIndex 3 },
        size: { width: parseIndex 4, height: parseIndex 5 }
    }
    where
        justOrDefault = fromMaybe 0
        parseIndex index = justOrDefault $ Int.fromString =<< (join $ groups!!index)

parseClaim :: Regex -> String -> Claim
parseClaim regex str = do
    case match regex str of
        Just groups -> parseGroups groups
        Nothing -> emptyClaim

getClaimPoints :: Claim -> Array Point
getClaimPoints claim = do
    let xs = claim.point.x .. (claim.point.x + claim.size.width - 1)
    let ys = claim.point.y .. (claim.point.y + claim.size.height - 1)
    Array.concat $ traverse { x: _, y: _ } xs <$> ys

placeClaim :: Fabric -> Claim -> Fabric
placeClaim fabric claim = do
    let points = getClaimPoints claim
    foldl markPoint fabric points
    where
        updateY y list = case Array.modifyAt y (List.Cons claim) list of
            Just newList -> newList
            _ -> list
        update x y fabric' = Array.modifyAt x (updateY y) fabric'
        markPoint fabric' {x, y} = case update x y fabric' of
            Just newFabric -> newFabric
            _ -> fabric'

placeClaims :: Array Claim -> Fabric
placeClaims claims = do
    let maxX = fromMaybe 0 $ maximum $ claimMaxX <$> claims
    let maxY = fromMaybe 0 $ maximum $ claimMaxY <$> claims
    let fabric = traverse (\_ _ -> Nil) (Array.range 0 maxX) <$> Array.range 0 maxY
    foldl placeClaim fabric claims
    where
        claimMaxX claim = claim.point.x + claim.size.width - 1
        claimMaxY claim = claim.point.y + claim.size.height - 1

findOverlaps :: Array Claim -> Array (List Claim)
findOverlaps claims = do
    Array.filter len1 $ Array.concat $ placeClaims claims
    where
        len1 list = (List.length list) > 1

parseClaims :: String -> Array Claim
parseClaims input = case claimRegex of
    Right pattern -> parseClaim pattern <$> splitLines input
    _ -> []

solve1 :: Array Claim -> Int
solve1 = Array.length <<< findOverlaps

part1 :: Program Solution
part1 = Just <$> show <$> solve1 <$> parseClaims <$> ask

solve2 :: Array Claim -> Set Claim
solve2 claims = do
    let overlaps = Array.concatMap Array.fromFoldable $ findOverlaps claims
    let overlapSet = Set.fromFoldable $ overlaps
    Set.difference (Set.fromFoldable claims) overlapSet

part2 :: Program Solution
part2 = Just <$> show <$> solve2 <$> parseClaims <$> ask
