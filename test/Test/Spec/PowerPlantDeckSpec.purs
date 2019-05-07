module Test.Spec.PowerPlantDeckSpec(powerPlantDeckSpec) where

import Prelude

import Data.Foldable (class Foldable, any)
import Data.Maybe (Maybe(..))
import PowerPlantDeck (newPowerPlantDeck, onTop, toList, remaining)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import PowerPlant (PowerPlant(..), cost)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

checkCost :: PowerPlant -> Int -> Aff Unit
checkCost (PowerPlant p) cost = p.cost `shouldEqual` cost

shouldNotMatchAny :: forall a f. Foldable f =>  f a -> (a -> Boolean)-> Aff Unit
shouldNotMatchAny f p = (any p f) `shouldEqual` false

powerPlantDeckSpec :: Spec Unit
powerPlantDeckSpec = do
  describe "PowerPlantDeck" do
    describe "newPowerPlantDeck" do
      it "correct power plants" do
        deck <- liftEffect $ newPowerPlantDeck 2
        (cost <$> onTop deck) `shouldEqual` Just 13
        (deck # toList) `shouldNotMatchAny` \(PowerPlant p) -> p.cost <= 10

      it "2 players" do
        deck <- liftEffect $ newPowerPlantDeck 2
        (remaining deck) `shouldEqual` 26

      it "3 players" do
        deck <- liftEffect $ newPowerPlantDeck 3
        (remaining deck) `shouldEqual` 26    

      it "4 players" do
        deck <- liftEffect $ newPowerPlantDeck 4
        (remaining deck) `shouldEqual` 30 

      it "5 players" do
        deck <- liftEffect $ newPowerPlantDeck 5
        (remaining deck) `shouldEqual` 34

      it "4 players" do
        deck <- liftEffect $ newPowerPlantDeck 6
        (remaining deck) `shouldEqual` 34     
