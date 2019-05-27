module Powergrid.NetworkMap (
  Area(..),
  City(..),
  NetworkMap(..),
  Path(..),
  shortestPath,
  cost,
  newNetworkMap
) where

import Prelude

import Data.Graph (AdjacencyList, Graph, fromAdjacencyList, vertices, weight)
import Data.Graph (shortestPath) as Graph
import Data.List (List(..), (:), singleton, filter, head)
import Data.Map (Map)
import Data.Map (fromFoldableWith, toUnfoldable) as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Powergrid.Util.List ((++), concat2)
  
newtype Area = Area String

instance eqArea :: Eq Area where
  eq (Area a) (Area b) = eq a b

data City = City String Area

instance ordCity :: Ord City where
  compare (City a _) (City b _) = compare a b

instance eqCity :: Eq City where
  eq (City a _) (City b _) = eq a b

instance showCity :: Show City where
  show (City id area) = id

newtype NetworkMap = NetworkMap (Graph City Int)

type Path a = List a

newNetworkMap :: AdjacencyList City Int -> NetworkMap
newNetworkMap connections = NetworkMap $ fromAdjacencyList $ connections ++ inverseAdjacencyList connections

shortestPath :: City -> City -> NetworkMap -> Maybe (Path City)
shortestPath from to (NetworkMap graph) = Graph.shortestPath from to graph

cost :: City -> City -> NetworkMap -> Maybe Int
cost from to (NetworkMap graph) = cost' <$> path
  where
    path :: Maybe (Path City)
    path = Graph.shortestPath from to graph
    cost' :: (Path City) -> Int
    cost' (first : remaining) = cost'' first remaining
    cost' Nil = 0
    cost'' :: City -> Path City -> Int
    cost'' prev (cur : remaining) = (fromMaybe 0 (weight prev cur graph)) + (cost'' cur remaining)
    cost'' _ Nil = 0
    
inverseAdjacencyList :: forall a w. Ord a => AdjacencyList a w -> AdjacencyList a w
inverseAdjacencyList as = Map.toUnfoldable grouped
  where
    flattenAdjacencyList = as >>= (\(Tuple from ws) -> (\(Tuple to w) -> Tuple from $ Tuple to w) <$> ws)
    inverseFlattened = (\(Tuple from (Tuple to w)) -> Tuple to $ singleton (Tuple from w)) <$> flattenAdjacencyList
    grouped :: Map a (List (Tuple a w))
    grouped = Map.fromFoldableWith concat2 $ inverseFlattened    