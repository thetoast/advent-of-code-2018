module Main where

import Prelude

import Data.Traversable (traverse, traverse_)
import Day1 as Day1
import Day2 as Day2
import Day3 as Day3
import Day4 as Day4
import Day5 as Day5
import Day6 as Day6
import Day7 as Day7
import Effect (Effect)
import Effect.Console (log)
import Foreign (unsafeFromForeign)
import JQuery as J
import Util (MainProgram, ProgramResult(..), runProgram)

type TestDay = {
    day :: Int,
    part1 :: MainProgram,
    part2 :: MainProgram
}

tests :: Array TestDay
tests = [
    {day: 1, part1: Day1.part1, part2: Day1.part2},
    {day: 2, part1: Day2.part1, part2: Day2.part2},
    {day: 3, part1: Day3.part1, part2: Day3.part2},
    {day: 4, part1: Day4.part1, part2: Day4.part2},
    {day: 5, part1: Day5.part1, part2: Day5.part2},
    {day: 6, part1: Day6.part1, part2: Day6.part2},
    {day: 7, part1: Day7.part1, part2: Day7.part2}
]

handleClick :: MainProgram -> J.JQueryEvent -> J.JQuery -> Effect Unit
handleClick func _ _ = do
  inputValue <- (J.select "#input" >>= J.getValue <#> unsafeFromForeign)
  res <- J.select "#results"
  case runProgram func inputValue {} of
    ProgramResult _ logs result -> do
      traverse_ log logs
      J.setText result res
    ProgramError logs error -> do
      traverse_ log logs
      traverse_ (\e -> J.setText ("Error: " <> e) res) error

createLink :: String -> MainProgram -> Effect J.JQuery
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
