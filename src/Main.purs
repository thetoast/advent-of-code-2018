module Main where

import Prelude
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import JQuery as J

import Day1 (day1)

type Solution = Effect String
type SolutionLink = Tuple String Solution
type EvalFunction = (Solution -> Effect Unit)

tests :: Array SolutionLink
tests = [
    Tuple "Day 1" day1
]

runAndPrintResultsTo :: J.JQuery -> Solution -> Effect Unit
runAndPrintResultsTo resultElem solution = do
  result <- solution
  J.setText result resultElem

renderTestLinkTo :: J.JQuery -> EvalFunction -> SolutionLink -> Effect Unit
renderTestLinkTo testsElem evalFunc testTuple = do
  link <- J.create "<a>"
  J.appendText (fst testTuple) link
  J.setAttr "href" "#" link
  J.on "click" (\_ _ -> evalFunc (snd testTuple)) link

  J.append link testsElem

main :: Effect Unit
main = J.ready $ do
  log "Loading app"
  testsDiv <- J.select "#tests"
  solutionDiv <- J.select "#results"
  traverse (renderTestLinkTo testsDiv (runAndPrintResultsTo solutionDiv)) tests
