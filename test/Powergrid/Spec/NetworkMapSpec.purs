module Powergrid.Spec.NetworkMapSpec(networkMapSpec) where

import Prelude

import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Powergrid.Map.Germany (augsburg, berlin, flensburg, fulda, germany, hamburg, hannover, kassel, kiel, muenchen, ne, nw, saarbruecken, se, sw, wuerzburg)
import Powergrid.NetworkMap (cost, restrict, shortestPath)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

networkMapSpec :: Spec Unit
networkMapSpec = do

  describe "NetworkMap" do
    describe "shortestPath" do
      it "multiple" do
        let path = shortestPath flensburg muenchen germany
        path `shouldEqual` (Just $ fromFoldable [flensburg, kiel, hamburg, hannover, kassel, fulda, wuerzburg, augsburg, muenchen])

      it "inverse" do
        (shortestPath kiel flensburg germany) `shouldEqual` (Just $ fromFoldable [kiel, flensburg])

    describe "cost" do
      it "direct" do
        (cost kiel flensburg germany) `shouldEqual` Just 4

      it "multiple" do
        (cost flensburg muenchen germany) `shouldEqual` Just 88

    describe "restrict" do
      it "reachable" do
        let restricted = restrict [ne, nw] germany
        (cost kiel berlin restricted) `shouldEqual` Just 34

      it "unreachable" do
        let restricted = restrict [ne, se] germany
        (cost berlin muenchen restricted) `shouldEqual` Nothing

      it "unreachable clusters" do
        let restricted = restrict [ne, nw, sw, se] germany
        (cost kiel berlin restricted) `shouldEqual` Just 34
        (cost saarbruecken muenchen restricted) `shouldEqual` Just 38
        (cost berlin muenchen restricted) `shouldEqual` Nothing