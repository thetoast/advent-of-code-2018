module Util where

import Data.String as String
import Data.String (Pattern(..))

splitLines :: String -> Array String
splitLines = String.split (Pattern "\n")
