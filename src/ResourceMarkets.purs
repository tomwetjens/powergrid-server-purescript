module ResourceMarkets (
  ResourceMarkets, 
  availableRT,
  capacityRT,
  newResourceMarkets
) where

import Prelude

import Data.Map (Map, unions, singleton, lookup)
import Data.Maybe (maybe)
import ResourceMarket (ResourceMarket, inc, newDefaultResourceMarket, newUraniumResourceMarket, available, capacity)
import ResourceType (ResourceType(..))

data ResourceMarkets = ResourceMarkets (Map ResourceType ResourceMarket)

instance showResourceMarkets :: Show ResourceMarkets where
  show (ResourceMarkets m) = show m

newResourceMarkets :: ResourceMarkets
newResourceMarkets = ResourceMarkets $ unions [
  singleton Coal (inc 24 newDefaultResourceMarket),
  singleton Oil (inc 18 newDefaultResourceMarket),
  singleton BioMass (inc 6 newDefaultResourceMarket),
  singleton Uranium (inc 2 newUraniumResourceMarket)
]

availableRT :: ResourceType -> ResourceMarkets -> Int
availableRT t (ResourceMarkets m) = maybe 0 available (lookup t m)

capacityRT :: ResourceType -> ResourceMarkets -> Int
capacityRT t (ResourceMarkets m) = maybe 0 capacity (lookup t m)

-- incRT :: ResourceType -> Int -> ResourceMarkets -> ResourceMarkets
-- incRT t n (ResourceMarkets market) = ResourceMarkets (maybe market (\mm -> insert t mm market) ((inc n) <$> (lookup t market)) market)
