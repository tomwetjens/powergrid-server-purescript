module Powergrid.ResourceMarket (
  ResourceMarket, 
  dec,
  inc,
  newDefaultResourceMarket, 
  newUraniumResourceMarket, 
  available, 
  cost, 
  capacity
) where

import Prelude

import Data.Foldable (foldl, foldr, sum)
import Data.List (List(..), (:), (..), fromFoldable, reverse)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Powergrid.Util.List ((++))

data Space = Space { capacity :: Int, cost :: Int, available :: Int }

instance showSpace :: Show Space where
  show (Space s) = show s

availableS :: Space -> Int
availableS (Space s) = s.available

capacityS :: Space -> Int
capacityS (Space s) = s.capacity

data ResourceMarket = ResourceMarket (List Space)

instance showResourceMarket :: Show ResourceMarket where
  show (ResourceMarket spaces) = show spaces

-- | Initializes new resource market with 8 spaces, each having capacity 3 where first space will have cost 1, the second space cost 2, etc.
newDefaultResourceMarket :: ResourceMarket
newDefaultResourceMarket = ResourceMarket ((\n -> Space { capacity: 3, cost: n, available: 0 }) <$> (1..8))

-- | Initializes new resource market with 8 spaces, each having capacity 1 where the first space will have cost 1, the second space cost 2, etc.
-- | and 4 additional spaces, each having capacity 1 where the additional spaces have cost 10, 12, 14, 16 respectively.
newUraniumResourceMarket :: ResourceMarket
newUraniumResourceMarket = ResourceMarket (
    ((\n -> Space { capacity: 1, cost: n, available: 0 }) <$> (1..8)) 
    ++ 
    ((\n -> Space { capacity: 1, cost: n, available: 0 }) <$> fromFoldable [10, 12, 14, 16]))

-- | Available resources in a market
available :: ResourceMarket -> Int
available (ResourceMarket spaces) = sum (availableS <$> spaces)

-- | Capacity in a market
capacity :: ResourceMarket -> Int
capacity (ResourceMarket spaces) = sum (capacityS <$> spaces)

-- | Cost to take a given amount from a market
cost :: ResourceMarket -> Int -> Maybe Int
cost (ResourceMarket spaces) amount = case (foldl _calc (Tuple amount 0) spaces) of
  (Tuple 0 cost) -> Just cost
  _ -> Nothing
  where 
    _calc :: (Tuple Int Int) -> Space -> (Tuple Int Int)
    _calc acc@(Tuple 0 _) _ = acc
    _calc (Tuple amountToTake cost) (Space s) = (Tuple (amountToTake - take) (cost + take * s.cost))
      where
        take = min s.available amountToTake

-- | Increase the available amount in a market
inc :: Int -> ResourceMarket -> ResourceMarket
inc amount (ResourceMarket spaces) = ResourceMarket (snd (foldr incS (Tuple amount Nil) spaces))
  where
    incS :: Space -> (Tuple Int (List Space)) -> Tuple Int (List Space)
    incS (Space s) (Tuple amountRemaining result) = Tuple (amountRemaining - amountCanInc) ((Space s { available = s.available + amountCanInc }) : result)
      where 
        amountCanInc = min (s.capacity - s.available) amountRemaining

-- | Decrease the available amount in market
dec :: Int -> ResourceMarket -> ResourceMarket
dec amount (ResourceMarket spaces) = ResourceMarket (reverse $ snd (foldl decS (Tuple amount Nil) spaces))
  where
    decS :: (Tuple Int (List Space)) -> Space -> Tuple Int (List Space)
    decS (Tuple amountRemaining result) (Space s) = Tuple (amountRemaining - amountCanDec) ((Space s { available = s.available - amountCanDec }) : result)
      where 
        amountCanDec = min s.available amountRemaining
