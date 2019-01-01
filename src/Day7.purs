module Day7 where

import Prelude

import Control.Monad.Except (throwError)
import Control.Monad.RWS (ask, get, put, tell)
import Data.Array.NonEmpty ((!!))
import Data.Either (fromRight)
import Data.Foldable (foldl, intercalate)
import Data.List (List)
import Data.List (fromFoldable, many, uncons) as List
import Data.Map (Map)
import Data.Map (alter, empty, keys, member, pop, size) as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set (delete, difference, empty, findMin, insert, member, singleton, size) as Set
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Util (MainProgram, Program, ProgramResult(..), except', log', runProgram)
import Util (splitLines) as Util

type Edge = String
type Edges = Set Edge
type Node = String
type NodeEdgesMap = Map Node Edges
type Day7State = {
  lines :: List String,
  outs  :: NodeEdgesMap,
  ins   :: NodeEdgesMap,
  heads :: Set Node
}
type Day7Program = Program Day7State

initialState :: String -> Day7State
initialState str = {
  lines: List.fromFoldable $ Util.splitLines str,
  outs: Map.empty,
  ins: Map.empty,
  heads: Set.empty
}

-- Step C must be finished before step A can begin.
edgeRegex :: Regex
edgeRegex = unsafePartial $ fromRight $ regex """Step (.+) must be finished before step (.+) can begin\.""" noFlags

parseLine :: Day7Program (Maybe { from :: String, to :: String })
parseLine = do
  state@{ lines } <- get
  case List.uncons lines of
    Just { head: line, tail: newLines } -> do
      matches <- except' "bad match" $ match edgeRegex line
      from <- except' "no match group 1" $ join $ matches !! 1
      to <- except' "no match group 2" $ join $ matches !! 2
      _ <- put state { lines = newLines }
      pure $ Just { from, to }
    Nothing -> pure Nothing

addEdge :: Node -> Node -> Day7Program Unit
addEdge from to = do
  state <- get
  put state {
    outs = Map.alter (addEdge' to) from state.outs,
    ins = Map.alter (addEdge' from) to state.ins
  }
  where
    addEdge' node (Just edges) = Just $ Set.insert node edges
    addEdge' node (Nothing) = Just $ Set.singleton node

updateHeads :: Node -> Node -> Day7Program Unit
updateHeads from to = do
  state <- get
  let fromHasParent = Map.member from state.ins
  let toInHeads  = Set.member to state.heads
  if not fromHasParent
    then put state { heads = Set.insert from state.heads }
    else pure unit
  if toInHeads
    then put state { heads = Set.delete to state.heads }
    else pure unit

findHeads :: NodeEdgesMap -> NodeEdgesMap -> Set Node
findHeads outs ins = Set.difference (Map.keys outs) (Map.keys ins)

parse :: Day7Program Unit
parse = do
  state@{ lines, outs, ins } <- get
  --log' $ "outs: " <> (show outs)
  --log' $ "ins : " <> (show ins)
  line <- parseLine
  case line of
    Just { from, to } -> do
      log' $ "adding edge from " <> from <> " to " <> to
      _ <- addEdge from to
      updateHeads from to
    Nothing -> throwError ["no more lines"]

removeNode :: Day7State -> Node -> Day7State
removeNode state node = do
  let updatedHeads = Set.delete node state.heads
  case Map.pop node state.outs of
    Just (Tuple children outs) -> do
      let newIns = foldl removeParents state.ins children
      let newHeads = foldl (updateHeads' newIns) updatedHeads children
      state { outs = outs, ins = newIns, heads = newHeads }
    Nothing -> state { heads = updatedHeads }
  where
    removeParents ins inNode = Map.alter removeParent inNode ins
    removeParent (Just parents) = do
      let newParents = Set.delete node parents
      if Set.size newParents == 0 then Nothing else Just newParents
    removeParent Nothing = Nothing
    updateHeads' ins heads child =
        if ((Map.size ins) > 0) && (Map.member child ins)
          then heads
          else Set.insert child heads

build :: Day7Program Node
build = do
  state@{ outs, ins, heads } <- get
  --log' $ "outs: " <> (show outs)
  --log' $ "ins : " <> (show ins)
  log' $ "heads: " <> (show heads)
  let next = Set.findMin heads
  case next of
    Just node -> do
      _ <- put $ removeNode state node
      log' $ "choose " <> node
      pure node
    Nothing -> throwError ["no more nodes"]


-- Solution: BITRAQVSGUWKXYHMZPOCDLJNFE
solve1 :: Day7Program String
solve1 = do
  log' "Part 1 started"
  _ <- List.many parse
  steps <- List.many build
  pure $ intercalate "" steps

part1 :: MainProgram
part1 = do
  input <- ask
  case runProgram solve1 input (initialState input) of
    ProgramResult _ logs result -> do
      tell logs
      pure result
    ProgramError logs errs -> do
      tell logs
      throwError errs

part2 :: MainProgram
part2 = pure $ "Part 2"
