module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

import Powergrid.Spec.GameSpec (gameSpec)
import Powergrid.Spec.NetworkMapSpec (networkMapSpec)
import Powergrid.Spec.PowerPlantDeckSpec (powerPlantDeckSpec)
import Powergrid.Spec.PowerPlantMarketSpec (powerPlantMarketSpec)
import Powergrid.Spec.ResourceMarketSpec (resourceMarketSpec)
import Powergrid.Spec.ResourceMarketsSpec (resourceMarketsSpec)

main :: Effect Unit
main = run [consoleReporter] do
  gameSpec
  networkMapSpec
  powerPlantDeckSpec
  powerPlantMarketSpec
  resourceMarketSpec
  resourceMarketsSpec