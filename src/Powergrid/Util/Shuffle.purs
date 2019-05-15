module Powergrid.Util.Shuffle(shuffle) where

import Prelude
import Data.Foldable (foldM)
import Data.List (List(Nil), singleton, concat, length, (:))
import Data.Pair (Pair(..))
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Random (random)

shuffle :: forall a. List a -> Effect (List a)
shuffle Nil = pure Nil
shuffle list@(e : Nil) = pure list
shuffle list = do
  pair <- splitRandomly list
  case pair of 
    (Pair sublist1 sublist2) -> do
      ls1 <- shuffle sublist1
      ls2 <- shuffle sublist2
      pure $ concat (ls1 : singleton ls2)

splitRandomly :: forall a. List a -> Effect (Pair (List a))
splitRandomly list = foldM putInEitherListRandomly (Pair Nil Nil) list where
  len = length list
  mid = len / 2
  putInEitherListRandomly :: Pair (List a) -> a -> Effect (Pair (List a))
  putInEitherListRandomly (Pair sublist1 sublist2) e = do
    r <- random
    let remaining1 = mid - length sublist1
    let remaining2 = len - mid - length sublist2
    pure if (r < toNumber remaining1 / toNumber (remaining1 + remaining2))
      then Pair (e : sublist1) sublist2 
      else Pair sublist1 (e : sublist2)