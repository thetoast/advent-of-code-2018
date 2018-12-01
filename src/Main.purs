module Main where

import Prelude
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Foreign (unsafeFromForeign)
import JQuery as J

import Day1 as Day1

type Solution = Effect String
type AdventFunction = String -> Solution
type AdventLink = Tuple String AdventFunction

tests :: Array AdventLink
tests = [
    Tuple "Day 1 - Part 1" Day1.part1,
    Tuple "Day 1 - Part 2" Day1.part2
]

runAndPrintResults :: Solution -> Effect Unit
runAndPrintResults solution = do
  result <- solution
  J.setText result =<< J.select "#results"

handleClick :: AdventFunction -> J.JQueryEvent -> J.JQuery -> Effect Unit
handleClick func _ _ = do
  inputValue <- (J.select "#input" >>= J.getValue <#> unsafeFromForeign)
  runAndPrintResults (func inputValue)

createLink :: AdventLink -> Effect J.JQuery
createLink adventLink = do
  link <- J.create "<a>"
  J.appendText (fst adventLink) link
  J.setAttr "href" "#" link
  J.on "click" (handleClick $ snd adventLink) link
  pure link

renderTestLink :: AdventLink -> Effect Unit
renderTestLink adventLink = do
  testsDiv <- J.select "#tests"
  link <- createLink adventLink
  br <- J.create "<br>"
  J.append link testsDiv
  J.append br testsDiv

main :: Effect Unit
main = J.ready $ do
  log "Loading app"
  traverse renderTestLink tests
