module Powergrid.Util.List where

import Data.List (List, concat, singleton, (:))

concat2 :: forall a. List a -> List a -> List a
concat2 a b = concat (a : (singleton b))

infixr 6 concat2 as ++