module Powergrid.ResourceMarkets (
  ResourceMarkets(..), 
  available,
  capacity,
  inc,
  dec,
  cost,
  newResourceMarkets
) where

import Prelude

import Data.Map (Map, unions, singleton, lookup, insert)
import Data.Maybe (Maybe, maybe)
import Powergrid.ResourceMarket (ResourceMarket, inc, dec, cost, newDefaultResourceMarket, newUraniumResourceMarket, available, capacity) as ResourceMarket
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

cost :: ResourceType -> ResourceMarkets -> Maybe Int
cost t (ResourceMarkets m) = (lookup t m) >>=  (flip ResourceMarket.cost) 1

inc :: Int -> ResourceType -> ResourceMarkets -> ResourceMarkets
inc amount t = withMarket t $ ResourceMarket.inc amount

dec :: Int -> ResourceType -> ResourceMarkets -> ResourceMarkets
dec amount t = withMarket t $ ResourceMarket.dec amount
  
withMarket :: ResourceType -> (ResourceMarket.ResourceMarket -> ResourceMarket.ResourceMarket) -> ResourceMarkets -> ResourceMarkets
withMarket t f markets@(ResourceMarkets m) = maybe markets ResourceMarkets updated
  where
    market = lookup t m
    updated = flip (insert t) m <$> f <$> market
