module Test.Main where

import Prelude
import Effect (Effect)

import Test.Day1 as Day1
import Test.Day2 as Day2
import Test.Day3 as Day3

main :: Effect Unit
main = do
    Day1.main
    Day2.main
    Day3.main
