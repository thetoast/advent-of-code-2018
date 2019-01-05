module Day10 where

import Prelude

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify_, put)
import Data.Array (modifyAt, updateAt, (..))
import Data.Array.NonEmpty ((!!))
import Data.Either (hush)
import Data.Foldable (foldl, foldr, intercalate)
import Data.List (List(..), many, manyRec, uncons, (:))
import Data.List (fromFoldable) as List
import Data.List.NonEmpty (NonEmptyList(..), head, toList)
import Data.Maybe (Maybe(..), fromJust)
import Data.NonEmpty (NonEmpty(..))
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)
import Partial.Unsafe (unsafePartial)
import Util (MainProgram, Point, Program, except', log', runSubprogram, splitLines, throwLog, unsafeInt)

type Star = {
  position :: Point,
  velocity :: Point
}
type Bounds = {
  maxX :: Int,
  maxY :: Int,
  minX :: Int,
  minY :: Int
}
type Day10State = {
  input :: List String,
  bounds :: Maybe Bounds,
  stars :: List Star,
  steps :: Int
}
type Day10Program = Program Day10State

initialState :: Day10State
initialState = {
  input: Nil,
  bounds: Nothing,
  stars: Nil,
  steps: 0
}

boundsFromStar :: Star -> Bounds
boundsFromStar { position: p } = { maxX: p.x, maxY: p.y, minX: p.x, minY: p.y }

updateBounds :: Maybe Bounds -> Star -> Bounds
updateBounds Nothing star = boundsFromStar star
updateBounds (Just bounds) star = updateBounds' bounds star

updateBounds' :: Bounds -> Star -> Bounds
updateBounds' {maxX, maxY, minX, minY} star ={
  maxX: max star.position.x maxX,
  maxY: max star.position.y maxY,
  minX: min star.position.x minX,
  minY: min star.position.y minY
}

determineBounds :: NonEmptyList Star -> Bounds
determineBounds stars =
  let initBounds = boundsFromStar (head stars)
   in foldl updateBounds' initBounds stars

height :: Bounds -> Int
height { maxY, minY } = maxY - minY

width :: Bounds -> Int
width { maxX, minX } = maxX - minX

--position=< 9,  1> velocity=< 0,  2>
starRegex :: Maybe Regex
starRegex
  = hush
  $ regex """position=<\s*(-?\d+),\s+(-?\d+)> velocity=<\s*(-?\d+),\s+(-?\d+)>""" noFlags

parseStar :: Regex -> Day10Program Star
parseStar pat = do
  { input } <- get
  case uncons input of
    Nothing -> throwError ["no more lines"]
    Just { head, tail } -> do
      modify_ (\s -> s { input = tail })
      m <- except' "bad match" (match pat head)
      posX <- except' "could not read x pos" $ unsafeInt <$> (join $ m !! 1)
      posY <- except' "could not read y pos" $ unsafeInt <$> (join $ m !! 2)
      velX <- except' "could not read x vel" $ unsafeInt <$> (join $ m !! 3)
      velY <- except' "could not read y vel" $ unsafeInt <$> (join $ m !! 4)
      pure { position: { x: posX, y: posY }, velocity: { x: velX, y: velY } }

moveStars :: Day10Program Unit
moveStars = do
  s@{ stars, bounds, steps } <- get
  let acc = { stars: Nil, bounds: Nothing }
  let { stars: newStars, bounds: newBounds } = foldr updateStars acc stars
  put s { stars = newStars, bounds = newBounds, steps = steps + 1 }

  case bounds of
    Nothing -> throwLog "bounds cannot be empty"
    Just bounds' ->
      case newBounds of
        Nothing -> throwLog "newBounds cannot be empty"
        Just newBounds' ->
          if (width bounds') < (width newBounds')
            then throwLog "bounds getting wider"
            else if (height bounds') < (height newBounds')
              then throwLog "bounds getting taller"
              else pure unit

  where
    updateStars { position, velocity } { stars, bounds } =
      let newPos = { x: position.x + velocity.x, y: position.y + velocity.y }
          newStar = { position: newPos, velocity }
          newBounds = Just $ updateBounds bounds newStar
       in { stars: newStar : stars, bounds: newBounds }

prettyPrint :: Bounds -> List Star -> String
prettyPrint { minX, maxX, minY, maxY } stars = do
  let initStarMap = (\_ -> (\_ -> ".") <$> (minX..maxX)) <$> (minY..maxY)
      normalize star@{ position: { x, y } } = { x: x - minX, y: y - minY }
      updateX x col = unsafePartial $ fromJust $ updateAt x "#" col
      updateStar starMap { x, y } =
        unsafePartial $ fromJust $ modifyAt y (\a -> updateX x a) starMap
      setStars stars' starMap = foldl updateStar starMap $ (normalize <$> stars')

  intercalate "\n" $ (intercalate "" <$> setStars stars initStarMap)

solve1 :: Day10Program String
solve1 = do
  --log' "Part 1 started"
  pat <- except' "bad regex" starRegex
  input <- ask
  modify_ (\s -> s { input = List.fromFoldable $ splitLines input })
  star <- parseStar pat
  rest <- many $ parseStar pat
  let stars = NonEmptyList (NonEmpty star rest)
  let bounds = determineBounds stars
  modify_ (\s -> s { stars = (toList stars), bounds = (Just bounds) })
  _ <- manyRec moveStars
  { bounds: finalBounds, stars: finalStars, steps } <- get

  pure $ "Steps: " <> show steps <> "\n" <> show finalBounds  <> "\n" <>
    prettyPrint (unsafePartial $ fromJust $ finalBounds) finalStars

part1 :: MainProgram
part1 = runSubprogram solve1 initialState

solve2 :: Day10Program String
solve2 = do
  log' "Part 2 started"
  pure "Part 2"

part2 :: MainProgram
part2 = runSubprogram solve2 initialState
