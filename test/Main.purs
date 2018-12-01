module Test.Main where

import Prelude
import Effect (Effect)

import Test.Day1 as Day1

main :: Effect Unit
main = do
    Day1.main
