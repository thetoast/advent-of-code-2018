module Util where

import Prelude

import Control.Monad.RWS (RWS, tell)
import Data.Foldable (foldl)
import Data.List (List)
import Data.List (singleton) as List
import Data.Map (Map)
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))

splitLines :: String -> Array String
splitLines = String.split (Pattern "\n")

splitChars :: String -> Array String
splitChars = String.split (Pattern "")

makePoint :: Int -> Int -> Point
makePoint x y = { x, y }
type Point = { x :: Int, y :: Int }
type Size = { width :: Int, height :: Int }
type Area = { point :: Point, size :: Size }

maxValue :: forall k v. Ord v => Map k v -> Maybe (Tuple k v)
maxValue m = maxValue' $ Map.toUnfoldable m

maxValue' :: forall k v. Ord v => Array (Tuple k v) -> Maybe (Tuple k v)
maxValue' a = foldl gt Nothing a
  where
    gt accM cur@(Tuple _ v2) = Just $ case accM of
      Nothing -> cur
      Just acc@(Tuple _ v1) -> if v1 > v2 then acc else cur

type Input = String
type Log = List String
newtype ProgramState = ProgramState { }
type Solution = Maybe String
type Program = RWS Input Log ProgramState

log' :: String -> Program Unit
log' = tell <<< List.singleton
