module Powergrid.Spec.PowerPlantMarketSpec(powerPlantMarketSpec) where

import Prelude

import Data.Foldable (foldr)
import Data.List (List(..), filter, fromFoldable, (..), (:), singleton)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import PowerPlant (PowerPlant(..), cost)
import PowerPlantDeck (PowerPlantDeck(..), defaultPowerPlants, onTop)
import PowerPlantMarket (PowerPlantMarket, newPowerPlantMarket, removeHighestFuture, removeLowerOrEqual, removeLowestAndReplace, removeLowestWithoutReplacement, takeActual, toPeriod3)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

takeFirst :: PowerPlantMarket -> PowerPlantMarket
takeFirst market = case market.actual of
  (first : _) -> takeActual market first
  Nil -> market

powerPlantMarketSpec :: Spec Unit
powerPlantMarketSpec = do

  let singletonDeck = (PowerPlantDeck (defaultPowerPlants # filter (\(PowerPlant p) -> p.cost == 13)))

  describe "PowerPlantMarket" do

    describe "newPowerPlantMarket" do 
      it "actual and future" do 
        let market = newPowerPlantMarket (PowerPlantDeck Nil)
        (cost <$> market.actual) `shouldEqual` (3..6)
        (cost <$> market.future) `shouldEqual` (7..10)

    describe "takeActual" do   
      let market = newPowerPlantMarket singletonDeck

      it "happy" do
        let result = takeFirst market
        (cost <$> result.actual) `shouldEqual` (4..7)
        (cost <$> result.future) `shouldEqual` fromFoldable [8, 9, 10, 13]

      it "not in actual" do
        case singletonDeck # onTop of
          Just other -> do
            let result = takeActual market other
            (cost <$> market.actual) `shouldEqual` (3..6)
            (cost <$> market.future) `shouldEqual` (7..10)     
          Nothing -> fail "deck empty"

      it "to period 3" do
        result <- liftEffect $ toPeriod3 market
        (cost <$> result.actual) `shouldEqual` (3..10)
        (cost <$> result.future) `shouldEqual` Nil

      it "exhausted" do
        let result = foldr (\i m -> takeFirst m) market (1..8)
        (cost <$> result.actual) `shouldEqual` (singleton 13)
        (cost <$> result.future) `shouldEqual` Nil

    describe "removeLowerOrEqual" do   
      let market = newPowerPlantMarket singletonDeck

      it "happy" do
        let result = removeLowerOrEqual 4 market
        (cost <$> result.actual) `shouldEqual` (5..8)
        (cost <$> result.future) `shouldEqual` fromFoldable [9, 10, 13]       

    describe "removeHighestFuture" do   
      let market = newPowerPlantMarket singletonDeck

      it "happy" do
        let result = removeHighestFuture market
        (cost <$> result.actual) `shouldEqual` (3..6)
        (cost <$> result.future) `shouldEqual` fromFoldable [7, 8, 9, 13]       

    describe "removeLowestAndReplace" do   
      let market = newPowerPlantMarket singletonDeck

      it "happy" do
        let result = removeLowestAndReplace market
        (cost <$> result.actual) `shouldEqual` (4..7)
        (cost <$> result.future) `shouldEqual` fromFoldable [8, 9, 10, 13]           

    describe "removeLowestWithoutReplacement" do   
      let market = newPowerPlantMarket singletonDeck

      it "happy" do
        let result = removeLowestWithoutReplacement market
        (cost <$> result.actual) `shouldEqual` (4..7)
        (cost <$> result.future) `shouldEqual` fromFoldable (8..10)               