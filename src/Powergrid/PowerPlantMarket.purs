module Powergrid.PowerPlantMarket (
  PowerPlantMarket, 
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

type PowerPlantMarket = { 
    deck :: PowerPlantDeck, 
    actual :: List PowerPlant, 
    future :: List PowerPlant
}

-- | Initializes a default market with the given `PowerPlantDeck` and the actual offering filled `[3,4,5,6]` 
-- | and the future offering filled `[7,8,9,10]`.
newPowerPlantMarket :: PowerPlantDeck -> PowerPlantMarket
newPowerPlantMarket deck = {
     deck: deck, 
     actual: initial # take 4, 
     future: initial # drop 4 } where
  initial = defaultPowerPlants # filter (\(PowerPlant p) -> p.cost <= 10) # sort

-- | Takes a `PowerPlant` from the actual offering and replaces it with the top one from the `PowerPlantDeck`.
takeActual :: PowerPlantMarket -> PowerPlant -> PowerPlantMarket
takeActual market powerPlant = if elem powerPlant market.actual then removeAndReplace market powerPlant else market

removeAndReplace :: PowerPlantMarket -> PowerPlant -> PowerPlantMarket
removeAndReplace market powerPlant = (removeWithReplacements market powerPlant replacements) { deck = newDeck }
  where 
    replacements = case market.deck of
      (PowerPlantDeck (onTop : _)) -> singleton onTop
      _ -> Nil
    newDeck = draw market.deck

removeWithReplacements :: PowerPlantMarket -> PowerPlant -> List PowerPlant -> PowerPlantMarket
removeWithReplacements market powerPlant replacements = market { actual = newActual, future = newFuture }
  where 
    newActualAndFuture = (delete powerPlant (market.actual ++ market.future)) ++ replacements # sort
    newActual = newActualAndFuture # take 4
    newFuture = newActualAndFuture # drop 4    

-- | Moves everything in the future offering to the actual offering and shuffles the deck for period 3
toPeriod3 :: PowerPlantMarket -> Effect PowerPlantMarket
toPeriod3 market = do
  shuffled <- shuffleDeck market.deck
  pure (market { actual = market.actual ++ market.future, future = Nil, deck = shuffled })

-- | Removes all power plants from the actual offering that have a cost lower than or equal to the given cost,
-- | and replaces them with new power plants from the deck (that have a cost higher than the given cost).  
removeLowerOrEqual :: Int -> PowerPlantMarket -> PowerPlantMarket
removeLowerOrEqual cost market = case market.actual of
  ((lowest@(PowerPlant p) : _)) -> if (p.cost <= cost) then removeLowerOrEqual cost (takeActual market lowest) else market
  Nil -> market

-- | Removes the highest power plant from the future offering, putting it back in the deck under the pile,
-- | and replaces it with a new power plant drawn from the deck.
removeHighestFuture :: PowerPlantMarket -> PowerPlantMarket
removeHighestFuture market = case highest of 
  Just p -> (removeAndReplace market p) { deck = putBack p market.deck }
  Nothing -> market
  where 
    highest = last market.future

-- | Removes lowest power plant from actual offering and replaces it with a new power plant drawn from the deck.    
removeLowestAndReplace :: PowerPlantMarket -> PowerPlantMarket
removeLowestAndReplace market = case market.actual of
  (lowest : _) -> removeAndReplace market lowest
  Nil -> market

removeLowestWithoutReplacement :: PowerPlantMarket -> PowerPlantMarket
removeLowestWithoutReplacement market = case market.actual of
  (lowest : _) -> removeWithReplacements market lowest Nil
  Nil -> market