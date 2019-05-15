module Powergrid.PowerPlantDeck(
  PowerPlantDeck(..),
  draw,
  putBack,
  onTop,
  remaining,
  shuffleDeck,
  defaultPowerPlants,
  newPowerPlantDeck
) where

import Prelude

import Data.List (List(Nil), (:), filter, drop, length, snoc)
import Data.Maybe (Maybe(..))
import Data.Set (singleton, union)
import Effect (Effect)
import Powergrid.PowerPlant (PowerPlant(..))
import Powergrid.ResourceType (ResourceType(..))
import Powergrid.Util.List ((++))
import Powergrid.Util.Shuffle (shuffle)

data PowerPlantDeck = PowerPlantDeck (List PowerPlant)

instance showPowerPlantDeck :: Show PowerPlantDeck where
  show (PowerPlantDeck powerPlants) = show powerPlants

draw :: PowerPlantDeck -> PowerPlantDeck
draw (PowerPlantDeck (_ : as)) = PowerPlantDeck as
draw deck@(PowerPlantDeck Nil) = deck

putBack :: PowerPlant -> PowerPlantDeck -> PowerPlantDeck
putBack p (PowerPlantDeck ps) = PowerPlantDeck (snoc ps p)

onTop :: PowerPlantDeck -> Maybe PowerPlant
onTop (PowerPlantDeck (p : ps)) = Just p
onTop (PowerPlantDeck Nil) = Nothing

remaining :: PowerPlantDeck -> Int
remaining (PowerPlantDeck ps) = length ps

shuffleDeck :: PowerPlantDeck -> Effect PowerPlantDeck
shuffleDeck (PowerPlantDeck powerPlants) = do
  shuffled <- shuffle powerPlants
  pure (PowerPlantDeck shuffled)

defaultPowerPlants :: List PowerPlant
defaultPowerPlants = 
    (PowerPlant { cost: 4, types: singleton Coal, requires: 2, powers: 1 })
  : (PowerPlant { cost: 8, types: singleton Coal, requires: 3, powers: 2 }) 
  : (PowerPlant { cost: 10, types: singleton Coal, requires: 2, powers: 2 })
  : (PowerPlant { cost: 15, types: singleton Coal, requires: 2, powers: 3 })
  : (PowerPlant { cost: 20, types: singleton Coal, requires: 3, powers: 5 })
  : (PowerPlant { cost: 25, types: singleton Coal, requires: 2, powers: 5 })
  : (PowerPlant { cost: 31, types: singleton Coal, requires: 3, powers: 6 })
  : (PowerPlant { cost: 36, types: singleton Coal, requires: 3, powers: 7 })
  : (PowerPlant { cost: 42, types: singleton Coal, requires: 2, powers: 6 })
  : (PowerPlant { cost: 6, types: singleton BioMass, requires: 1, powers: 1 })
  : (PowerPlant { cost: 14, types: singleton BioMass, requires: 2, powers: 2 })
  : (PowerPlant { cost: 19, types: singleton BioMass, requires: 2, powers: 3 })
  : (PowerPlant { cost: 24, types: singleton BioMass, requires: 2, powers: 4 })
  : (PowerPlant { cost: 30, types: singleton BioMass, requires: 3, powers: 6 })
  : (PowerPlant { cost: 38, types: singleton BioMass, requires: 3, powers: 7 })
  : (PowerPlant { cost: 11, types: singleton Uranium, requires: 1, powers: 2 })
  : (PowerPlant { cost: 17, types: singleton Uranium, requires: 1, powers: 2 })
  : (PowerPlant { cost: 23, types: singleton Uranium, requires: 1, powers: 3 })
  : (PowerPlant { cost: 28, types: singleton Uranium, requires: 1, powers: 4 })
  : (PowerPlant { cost: 34, types: singleton Uranium, requires: 1, powers: 5 })
  : (PowerPlant { cost: 39, types: singleton Uranium, requires: 1, powers: 6 })
  : (PowerPlant { cost: 3, types: singleton Oil, requires: 2, powers: 1 })
  : (PowerPlant { cost: 7, types: singleton Oil, requires: 3, powers: 2 })
  : (PowerPlant { cost: 9, types: singleton Oil, requires: 1, powers: 1 })
  : (PowerPlant { cost: 16, types: singleton Oil, requires: 2, powers: 3 })
  : (PowerPlant { cost: 26, types: singleton Oil, requires: 2, powers: 5 })
  : (PowerPlant { cost: 32, types: singleton Oil, requires: 3, powers: 6 })
  : (PowerPlant { cost: 35, types: singleton Oil, requires: 1, powers: 5 })
  : (PowerPlant { cost: 40, types: singleton Oil, requires: 2, powers: 6 })
  : (PowerPlant { cost: 5, types: (union (singleton Coal) (singleton Oil)), requires: 2, powers: 1 })
  : (PowerPlant { cost: 12, types: (union (singleton Coal) (singleton Oil)), requires: 2, powers: 2 })
  : (PowerPlant { cost: 21, types: (union (singleton Coal) (singleton Oil)), requires: 2, powers: 4 })
  : (PowerPlant { cost: 29, types: (union (singleton Coal) (singleton Oil)), requires: 1, powers: 3 })
  : (PowerPlant { cost: 96, types: (union (singleton Coal) (singleton Oil)), requires: 3, powers: 7 })
  : (PowerPlant { cost: 13, types: singleton Wind, requires: 0, powers: 1 })
  : (PowerPlant { cost: 18, types: singleton Wind, requires: 0, powers: 2 })
  : (PowerPlant { cost: 22, types: singleton Wind, requires: 0, powers: 2 })
  : (PowerPlant { cost: 27, types: singleton Wind, requires: 0, powers: 3 })
  : (PowerPlant { cost: 33, types: singleton Wind, requires: 0, powers: 4 })
  : (PowerPlant { cost: 37, types: singleton Wind, requires: 0, powers: 4 })
  : (PowerPlant { cost: 44, types: singleton Wind, requires: 0, powers: 5 })
  : (PowerPlant { cost: 50, types: singleton Wind, requires: 0, powers: 6 })
  : Nil

-- Initializes a shuffled deck for the default set of power plants and the given number of players, 
-- with `3-10` removed and `13` on top.
newPowerPlantDeck :: Int -> Effect PowerPlantDeck
newPowerPlantDeck numberOfPlayers = do
  shuffled <- defaultPowerPlants 
    # filter (\(PowerPlant p) -> p.cost > 10 && p.cost /= 13) 
    # shuffle
  let numberToDrop | numberOfPlayers <= 3 = 8
                   | numberOfPlayers == 4 = 4
                   | otherwise = 0
  let powerPlants = (defaultPowerPlants # filter (\(PowerPlant p) -> p.cost == 13)) ++ (drop numberToDrop shuffled)
  pure (PowerPlantDeck powerPlants)
