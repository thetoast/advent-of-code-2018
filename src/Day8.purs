module Day8 where

import Prelude

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, gets, modify_, put)
import Control.MonadZero (guard)
import Data.Array ((..))
import Data.Array (drop, index) as Array
import Data.Foldable (sum)
import Data.Int (fromString) as Int
import Data.List (List(..), (!!))
import Data.List (drop, fromFoldable, length, take) as List
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Util (MainProgram, Program, log', runSubprogram)
import Util (splitWords) as Util

type Metadata = Int
type NodeId = Int
newtype Node = Node {
  metadata :: List Metadata,
  children :: Array Node,
  value :: Int
}
instance showNode :: Show Node where
  show (Node { metadata, children}) =
    "Metadata: " <> (show metadata) <> "\n" <>
    case children of
      [] -> ""
      _ -> "Children: \n" <> show children

type Day8State = List Int
type Day8Program = Program Day8State

initialState :: Day8State
initialState = Nil

splitInput :: Day8Program Unit
splitInput = do
  words <- Util.splitWords <$> ask
  log' $ "Input words: " <> (show words)

  case traverse Int.fromString words of
    Just ints -> do
      log' $ "Input ints: " <> (show ints)
      put $ List.fromFoldable ints
    Nothing -> throwError ["unable to parse input"]

calculateNodeValue :: List Metadata -> Array Node -> Int
calculateNodeValue metadata children = case children of
  [] -> sum metadata
  _ -> sum $ (getValue <$> metadata)
  where
    getValue n = case Array.index children (n-1) of
      Just (Node node) -> node.value
      Nothing -> 0

parseNode :: Day8Program Node
parseNode = do
  log' "Parsing node"
  input <- get

  guard $ List.length input >= 1

  let numChildren = unsafePartial $ fromJust $ input !! 0
  let numMetadata = unsafePartial $ fromJust $ input !! 1
  modify_ $ List.drop 2
  log' $
    "#Children: " <> (show numChildren) <>
    " #Metadata: " <> (show numMetadata)

  children <- traverse (\_ -> parseNode) (Array.drop 1 (0..numChildren))
  metadata <- gets $ List.take numMetadata
  let value = calculateNodeValue metadata children

  modify_ $ List.drop numMetadata

  pure $ Node { metadata, children, value }

sumMetadata :: Node -> Int
sumMetadata (Node node) = sum node.metadata + sum (sumMetadata <$> node.children)

solve1 :: Day8Program String
solve1 = do
  log' "Part 1 started"
  _ <- splitInput
  head <- parseNode
  log' $ show head

  pure $ show $ sumMetadata head

part1 :: MainProgram
part1 = runSubprogram solve1 initialState

solve2 :: Day8Program String
solve2 = do
  log' "Part 2 started"
  _ <- splitInput
  (Node head) <- parseNode

  pure $ show $ head.value

part2 :: MainProgram
part2 = runSubprogram solve2 initialState
