module Powergrid.Player where

import Prelude

import Data.Map (Map)
import Data.Map (empty) as Map
import Data.Set (Set)
import Data.Set (empty) as Set
import Powergrid.PowerPlant (PowerPlant)
import Powergrid.ResourceType (ResourceType)

data Player = Player {
    name :: String,
    balance :: Int,
    powerPlants :: Set PowerPlant,
    resources :: Map ResourceType Int
}

instance showPlayer :: Show Player where
  show (Player player) = "Player:" <> 
    " name=" <> player.name <> 
    " balance=" <> show player.balance <> 
    " powerPlants=" <> show player.powerPlants <> 
    " resources=" <> show player.resources

derive instance eqPlayer :: Eq Player

newPlayer :: String -> Player
newPlayer name = Player { 
    name, 
    balance: 50, 
    powerPlants: Set.empty, 
    resources: Map.empty 
  }
