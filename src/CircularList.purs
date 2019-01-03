module CircularList
  ( CircularList
  , empty
  , singleton
  , insertRight
  , getRight
  , shiftRight
  , shiftRightN
  , insertLeft
  , getLeft
  , shiftLeft
  , shiftLeftN
  , getCurrent
  , popCurrent
  , toList
  ) where

import Prelude

import Data.Array ((..))
import Data.Foldable (foldl, intercalate)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map (insert, lookup, singleton) as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

data CircularList a
  = CircularList {
      lefts :: Map a a,
      rights :: Map a a,
      current :: a
    }
  | Empty
instance showCircularList :: (Ord a, Show a) => Show (CircularList a) where
  show Empty = "[...]"
  show l@(CircularList _) =
    let list = toList l
     in "[ " <> (intercalate ", " (show <$> list)) <> ", ... ]"

empty :: forall a. Ord a => CircularList a
empty = Empty

singleton :: forall a. Ord a => a -> CircularList a
singleton a = CircularList {
  lefts: Map.singleton a a,
  rights: Map.singleton a a,
  current: a
}

safeLookup :: forall a. Ord a => a -> Map a a -> a
safeLookup a map = unsafePartial $ fromJust $ Map.lookup a map

insertRight :: forall a. Ord a => a -> CircularList a -> CircularList a
insertRight a Empty = singleton a
insertRight a (CircularList { lefts, rights, current }) =
  let left = safeLookup current lefts
      right = safeLookup current rights
   in CircularList {
    lefts: Map.insert a current (Map.insert right a lefts),
    rights: Map.insert current a (Map.insert a right rights),
    current: a
  }

insertLeft :: forall a. Ord a => a -> CircularList a -> CircularList a
insertLeft a Empty = singleton a
insertLeft a (CircularList { lefts, rights, current }) =
  let left = safeLookup current lefts
      right = safeLookup current rights
   in CircularList {
     lefts: Map.insert current a (Map.insert a left lefts),
     rights: Map.insert a current (Map.insert left a rights),
     current: a
   }

getCurrent :: forall a. Ord a => CircularList a -> Maybe a
getCurrent Empty = Nothing
getCurrent (CircularList { current }) = Just current

popCurrent :: forall a. Ord a => CircularList a -> Maybe (Tuple a (CircularList a))
popCurrent Empty = Nothing
popCurrent (CircularList { rights, lefts, current }) =
  let left = safeLookup current lefts
      right = safeLookup current rights
      newList = CircularList {
        lefts: Map.insert right left lefts,
        rights: Map.insert left right rights,
        current: right
      }
   in Just $ (Tuple current newList)

getRight :: forall a. Ord a => CircularList a -> Maybe a
getRight Empty = Nothing
getRight (CircularList { rights, current }) = Just $ safeLookup current rights

shiftRight :: forall a. Ord a => CircularList a -> CircularList a
shiftRight Empty = Empty
shiftRight (l@CircularList a@{ rights, current }) =
  CircularList a{current = (safeLookup current rights)}

shiftRightN :: forall a. Ord a => Int -> CircularList a -> CircularList a
shiftRightN n Empty = Empty
shiftRightN n (l@CircularList _)
  | n == 0 = l
  | otherwise = foldl (\a _ -> shiftRight a) l (0..(n-1))

getLeft :: forall a. Ord a => CircularList a -> Maybe a
getLeft Empty = Nothing
getLeft (CircularList { lefts, current }) = Just $ safeLookup current lefts

shiftLeft :: forall a. Ord a => CircularList a -> CircularList a
shiftLeft Empty = Empty
shiftLeft (l@CircularList a@{ lefts, current }) =
  CircularList a{current = (safeLookup current lefts)}

shiftLeftN :: forall a. Ord a => Int -> CircularList a -> CircularList a
shiftLeftN n Empty = Empty
shiftLeftN n (l@CircularList _)
  | n == 0 = l
  | otherwise = foldl (\a _ -> shiftLeft a) l (0..(n-1))

toList :: forall a. Ord a => CircularList a -> List a
toList Empty = Nil
toList (CircularList { rights, current }) =
  let getRightList n | n == current = Nil
                     | otherwise    = n : (getRightList (safeLookup n rights))
   in current : getRightList (safeLookup current rights)
