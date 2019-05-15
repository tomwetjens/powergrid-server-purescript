module Powergrid.PowerPlant where

import Prelude
import Data.Set (Set)
import Powergrid.ResourceType (ResourceType)

data PowerPlant = PowerPlant { cost :: Int, requires :: Int, types :: Set ResourceType, powers :: Int }

instance showPowerPlant :: Show PowerPlant where
  show (PowerPlant p) = show p.cost

instance eqPowerPlant :: Eq PowerPlant where
  eq (PowerPlant a) (PowerPlant b) = eq a.cost b.cost

instance ordPowerPlant :: Ord PowerPlant where
  compare (PowerPlant a) (PowerPlant b) = compare a.cost b.cost

cost :: PowerPlant -> Int
cost (PowerPlant p) = p.cost