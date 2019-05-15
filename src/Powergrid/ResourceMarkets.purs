module Powergrid.ResourceMarkets (
  ResourceMarkets(..), 
  available,
  capacity,
  newResourceMarkets
) where

import Prelude

import Data.Map (Map, unions, singleton, lookup)
import Data.Maybe (maybe)
import Powergrid.ResourceMarket (ResourceMarket, inc, newDefaultResourceMarket, newUraniumResourceMarket, available, capacity) as ResourceMarket
import Powergrid.ResourceType (ResourceType(..))

data ResourceMarkets = ResourceMarkets (Map ResourceType ResourceMarket.ResourceMarket)

instance showResourceMarkets :: Show ResourceMarkets where
  show (ResourceMarkets m) = show m

newResourceMarkets :: ResourceMarkets
newResourceMarkets = ResourceMarkets $ unions [
  singleton Coal (ResourceMarket.inc 24 ResourceMarket.newDefaultResourceMarket),
  singleton Oil (ResourceMarket.inc 18 ResourceMarket.newDefaultResourceMarket),
  singleton BioMass (ResourceMarket.inc 6 ResourceMarket.newDefaultResourceMarket),
  singleton Uranium (ResourceMarket.inc 2 ResourceMarket.newUraniumResourceMarket)
]

available :: ResourceType -> ResourceMarkets -> Int
available t (ResourceMarkets m) = maybe 0 ResourceMarket.available (lookup t m)

capacity :: ResourceType -> ResourceMarkets -> Int
capacity t (ResourceMarkets m) = maybe 0 ResourceMarket.capacity (lookup t m)

-- incRT :: ResourceType -> Int -> ResourceMarkets -> ResourceMarkets
-- incRT t n (ResourceMarkets market) = ResourceMarkets (maybe market (\mm -> insert t mm market) ((inc n) <$> (lookup t market)) market)
