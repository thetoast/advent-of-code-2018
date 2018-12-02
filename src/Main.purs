module Main where

import Prelude
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Foreign (unsafeFromForeign)
import JQuery as J

import Day1 as Day1
import Day2 as Day2

type Solution = Effect String
type AdventFunction = String -> Solution
type TestDay = {
    day :: Int,
    part1 :: AdventFunction,
    part2 :: AdventFunction
}

tests :: Array TestDay
tests = [
    {day: 1, part1: Day1.part1, part2: Day1.part2},
    {day: 2, part1: Day2.part1, part2: Day2.part2}
]

runAndPrintResults :: Solution -> Effect Unit
runAndPrintResults solution = do
  result <- solution
  J.setText result =<< J.select "#results"

handleClick :: AdventFunction -> J.JQueryEvent -> J.JQuery -> Effect Unit
handleClick func _ _ = do
  inputValue <- (J.select "#input" >>= J.getValue <#> unsafeFromForeign)
  runAndPrintResults (func inputValue)

createLink :: String -> AdventFunction -> Effect J.JQuery
createLink name func = do
  link <- J.create "<a>"
  J.appendText name link
  J.setAttr "href" "#" link
  J.setAttr "class" "test-link" link
  J.on "click" (handleClick func) link
  pure link

renderTestLinks :: TestDay -> Effect Unit
renderTestLinks day = do
  testsDiv <- J.select "#tests"
  let dayText = "Day" <> (show day.day) <> " - "
  part1 <- createLink "[Part 1]" day.part1
  part2 <- createLink "[Part 2]" day.part2
  br <- J.create "<br>"
  J.appendText dayText testsDiv
  J.append part1 testsDiv
  J.append part2 testsDiv
  J.append br testsDiv

main :: Effect Unit
main = J.ready $ do
  log "Loading app"
  traverse renderTestLinks tests
