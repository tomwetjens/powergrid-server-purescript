module Powergrid.PowerPlantMarket (
  PowerPlantMarket(..), 
  actual,
  newPowerPlantMarket, 
  takeActual, 
  toPeriod3, 
  removeLowerOrEqual, 
  removeHighestFuture,
  removeLowestAndReplace,
  removeLowestWithoutReplacement
) where

import Prelude

import Data.List (List(..), (:), delete, drop, elem, filter, last, singleton, sort, take)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Powergrid.Util.List ((++))
import Powergrid.PowerPlant (PowerPlant(..))
import Powergrid.PowerPlantDeck (PowerPlantDeck(..), defaultPowerPlants, draw, putBack, shuffleDeck)

data PowerPlantMarket = PowerPlantMarket { 
  deck :: PowerPlantDeck, 
  actual :: List PowerPlant, 
  future :: List PowerPlant
}

-- | Initializes a default market with the given `PowerPlantDeck` and the actual offering filled `[3,4,5,6]` 
-- | and the future offering filled `[7,8,9,10]`.
newPowerPlantMarket :: PowerPlantDeck -> PowerPlantMarket
newPowerPlantMarket deck = PowerPlantMarket {
     deck: deck, 
     actual: initial # take 4, 
     future: initial # drop 4 } where
  initial = defaultPowerPlants # filter (\(PowerPlant p) -> p.cost <= 10) # sort

actual :: PowerPlantMarket -> List PowerPlant
actual (PowerPlantMarket market) = market.actual

-- | Takes a `PowerPlant` from the actual offering and replaces it with the top one from the `PowerPlantDeck`.
takeActual :: PowerPlantMarket -> PowerPlant -> PowerPlantMarket
takeActual market@(PowerPlantMarket m) powerPlant = if elem powerPlant m.actual 
  then removeAndReplace market powerPlant 
  else market

removeAndReplace :: PowerPlantMarket -> PowerPlant -> PowerPlantMarket
removeAndReplace market@(PowerPlantMarket m) powerPlant = PowerPlantMarket $ replaced { deck = newDeck }
  where 
    replacements = case m.deck of
      (PowerPlantDeck (onTop : _)) -> singleton onTop
      _ -> Nil
    newDeck = draw m.deck
    (PowerPlantMarket replaced) = removeWithReplacements market powerPlant replacements

removeWithReplacements :: PowerPlantMarket -> PowerPlant -> List PowerPlant -> PowerPlantMarket
removeWithReplacements (PowerPlantMarket m) powerPlant replacements = PowerPlantMarket $ m { actual = newActual, future = newFuture }
  where 
    newActualAndFuture = (delete powerPlant (m.actual ++ m.future)) ++ replacements # sort
    newActual = newActualAndFuture # take 4
    newFuture = newActualAndFuture # drop 4    

-- | Moves everything in the future offering to the actual offering and shuffles the deck for period 3
toPeriod3 :: PowerPlantMarket -> Effect PowerPlantMarket
toPeriod3 (PowerPlantMarket m) = do
  shuffled <- shuffleDeck m.deck
  pure $ PowerPlantMarket $ m { actual = m.actual ++ m.future, future = Nil, deck = shuffled }

-- | Removes all power plants from the actual offering that have a cost lower than or equal to the given cost,
-- | and replaces them with new power plants from the deck (that have a cost higher than the given cost).  
removeLowerOrEqual :: Int -> PowerPlantMarket -> PowerPlantMarket
removeLowerOrEqual cost market@(PowerPlantMarket m) = case m.actual of
  ((lowest@(PowerPlant p) : _)) -> if (p.cost <= cost) 
    then removeLowerOrEqual cost (takeActual market lowest) 
    else market
  Nil -> market

-- | Removes the highest power plant from the future offering, putting it back in the deck under the pile,
-- | and replaces it with a new power plant drawn from the deck.
removeHighestFuture :: PowerPlantMarket -> PowerPlantMarket
removeHighestFuture market@(PowerPlantMarket m) = case highest of 
  Just p -> PowerPlantMarket $ case removeAndReplace market p of 
    (PowerPlantMarket replaced) -> replaced { deck = putBack p m.deck }
  Nothing -> market
  where 
    highest = last m.future

-- | Removes lowest power plant from actual offering and replaces it with a new power plant drawn from the deck.    
removeLowestAndReplace :: PowerPlantMarket -> PowerPlantMarket
removeLowestAndReplace market@(PowerPlantMarket m) = case m.actual of
  (lowest : _) -> removeAndReplace market lowest
  Nil -> market

removeLowestWithoutReplacement :: PowerPlantMarket -> PowerPlantMarket
removeLowestWithoutReplacement market@(PowerPlantMarket m) = case m.actual of
  (lowest : _) -> removeWithReplacements market lowest Nil
  Nil -> market