module Main where

import Prelude

import Effect (Effect)
import Powergrid.Server (runServer)

main :: Effect Unit
main = runServer