module Powergrid.Spec.PowerPlantMarketSpec(powerPlantMarketSpec) where

import Prelude

import Data.Foldable (foldr)
import Data.List (List(..), filter, fromFoldable, (..), (:), singleton)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Powergrid.PowerPlant (PowerPlant(..), cost)
import Powergrid.PowerPlantDeck (PowerPlantDeck(..), defaultPowerPlants, onTop)
import Powergrid.PowerPlantMarket (PowerPlantMarket(..), newPowerPlantMarket, removeHighestFuture, removeLowerOrEqual, removeLowestAndReplace, removeLowestWithoutReplacement, takeActual, toPeriod3)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

takeFirst :: PowerPlantMarket -> PowerPlantMarket
takeFirst market@(PowerPlantMarket m) = case m.actual of
  (first : _) -> takeActual market first
  Nil -> market

powerPlantMarketSpec :: Spec Unit
powerPlantMarketSpec = do

  let singletonDeck = (PowerPlantDeck (defaultPowerPlants # filter (\(PowerPlant p) -> p.cost == 13)))

  describe "PowerPlantMarket" do

    describe "newPowerPlantMarket" do 
      it "actual and future" do 
        let (PowerPlantMarket result) = newPowerPlantMarket (PowerPlantDeck Nil)
        (cost <$> result.actual) `shouldEqual` (3..6)
        (cost <$> result.future) `shouldEqual` (7..10)

    describe "takeActual" do   
      let market = newPowerPlantMarket singletonDeck

      it "happy" do
        let (PowerPlantMarket result) = takeFirst market
        (cost <$> result.actual) `shouldEqual` (4..7)
        (cost <$> result.future) `shouldEqual` fromFoldable [8, 9, 10, 13]

      it "not in actual" do
        case singletonDeck # onTop of
          Just other -> do
            let (PowerPlantMarket result) = takeActual market other
            (cost <$> result.actual) `shouldEqual` (3..6)
            (cost <$> result.future) `shouldEqual` (7..10)     
          Nothing -> fail "deck empty"

      it "to period 3" do
        (PowerPlantMarket result) <- liftEffect $ toPeriod3 market
        (cost <$> result.actual) `shouldEqual` (3..10)
        (cost <$> result.future) `shouldEqual` Nil

      it "exhausted" do
        let (PowerPlantMarket result) = foldr (\i m -> takeFirst m) market (1..8)
        (cost <$> result.actual) `shouldEqual` (singleton 13)
        (cost <$> result.future) `shouldEqual` Nil

    describe "removeLowerOrEqual" do   
      let market = newPowerPlantMarket singletonDeck

      it "happy" do
        let (PowerPlantMarket result) = removeLowerOrEqual 4 market
        (cost <$> result.actual) `shouldEqual` (5..8)
        (cost <$> result.future) `shouldEqual` fromFoldable [9, 10, 13]       

    describe "removeHighestFuture" do   
      let market = newPowerPlantMarket singletonDeck

      it "happy" do
        let (PowerPlantMarket result) = removeHighestFuture market
        (cost <$> result.actual) `shouldEqual` (3..6)
        (cost <$> result.future) `shouldEqual` fromFoldable [7, 8, 9, 13]       

    describe "removeLowestAndReplace" do   
      let market = newPowerPlantMarket singletonDeck

      it "happy" do
        let (PowerPlantMarket result) = removeLowestAndReplace market
        (cost <$> result.actual) `shouldEqual` (4..7)
        (cost <$> result.future) `shouldEqual` fromFoldable [8, 9, 10, 13]           

    describe "removeLowestWithoutReplacement" do   
      let market = newPowerPlantMarket singletonDeck

      it "happy" do
        let (PowerPlantMarket result) = removeLowestWithoutReplacement market
        (cost <$> result.actual) `shouldEqual` (4..7)
        (cost <$> result.future) `shouldEqual` fromFoldable (8..10)               