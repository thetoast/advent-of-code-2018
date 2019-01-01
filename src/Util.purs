module Util where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Except (ExceptT, except, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Writer (class MonadTell, WriterT, runWriterT, tell)
import Data.Either (Either(..), note)
import Data.Foldable (foldl)
import Data.Identity (Identity)
import Data.List (List)
import Data.List (singleton) as List
import Data.Map (Map)
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
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
type Logs = List String
type Errors = Array String
type Solution = String
type Program s = StateT s (ExceptT Errors (WriterT Logs (ReaderT Input Identity)))
type MainState = { }
type MainProgram = Program MainState String
data ProgramResult s a
  = ProgramError Logs Errors
  | ProgramResult s Logs a

--runParser p s = runIdentity $ runExceptT $ runWriterT $ runStateT p s
runProgram :: forall s a. Program s a -> String -> s -> ProgramResult s a
runProgram p r s = do
  let es = runStateT p s
  let ws = runExceptT es
  let rs = runWriterT ws
  let res = (runReaderT rs) r
  case unwrap res of
       Tuple (Left e) w -> ProgramError w e
       Tuple (Right (Tuple a s')) w -> ProgramResult s' w a

log' :: forall m. MonadTell Logs m => String -> m Unit
log' = tell <<< List.singleton

except' :: forall s a. String -> Maybe a -> Program s a
except' msg a = lift $ except $ note [msg] a
