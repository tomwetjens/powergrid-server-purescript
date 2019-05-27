module Powergrid.Spec.NetworkMapSpec(networkMapSpec) where

import Prelude

import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Powergrid.NetworkMap (City(..), Area(..), shortestPath, cost)
import Powergrid.Map.Germany (germany)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

networkMapSpec :: Spec Unit
networkMapSpec = do

  describe "NetworkMap" do
    let networkMap = germany
    let flensburg = City "flensburg" (Area "nw")
    let muenchen = City "muenchen" (Area "se")
    let kiel = City "kiel" (Area "nw")
  
    describe "shortestPath" do
      it "multiple" do
        let path = shortestPath flensburg muenchen networkMap
        let ids = (map (\(City id _) -> id)) <$> path
        ids `shouldEqual` (Just $ fromFoldable ["flensburg", "kiel", "hamburg", "hannover", "kassel", "fulda", "wuerzburg", "augsburg", "muenchen"])

      it "inverse" do
        (shortestPath kiel flensburg networkMap) `shouldEqual` (Just $ fromFoldable [kiel, flensburg])

    describe "cost" do
      it "direct" do
        (cost kiel flensburg networkMap) `shouldEqual` Just 4

      it "multiple" do
        (cost flensburg muenchen networkMap) `shouldEqual` Just 88