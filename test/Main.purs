module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

import Test.Spec.PowerPlantDeckSpec (powerPlantDeckSpec)
import Test.Spec.PowerPlantMarketSpec (powerPlantMarketSpec)
import Test.Spec.ResourceMarketSpec (resourceMarketSpec)
import Test.Spec.ResourceMarketsSpec (resourceMarketsSpec)

main :: Effect Unit
main = run [consoleReporter] do
  powerPlantDeckSpec
  powerPlantMarketSpec
  resourceMarketSpec
  resourceMarketsSpec