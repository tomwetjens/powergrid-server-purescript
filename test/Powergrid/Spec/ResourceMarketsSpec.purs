module Powergrid.Spec.ResourceMarketsSpec(resourceMarketsSpec) where

import Prelude

import Data.Maybe (Maybe(..))
import Powergrid.ResourceType (ResourceType(..))
import Powergrid.ResourceMarkets (newResourceMarkets, available, capacity, inc, dec, cost)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

resourceMarketsSpec :: Spec Unit
resourceMarketsSpec = do

  describe "ResourceMarkets" do

    describe "newResourceMarkets" do 
      let markets = newResourceMarkets

      it "available" do   
        available Coal markets `shouldEqual` 24
        available Oil markets `shouldEqual` 18
        available BioMass markets `shouldEqual` 6
        available Uranium markets `shouldEqual` 2

      it "capacity" do   
        capacity Coal markets `shouldEqual` 24
        capacity Oil markets `shouldEqual` 24
        capacity BioMass markets `shouldEqual` 24
        capacity Uranium markets `shouldEqual` 12

    describe "cost" do 
      let markets = newResourceMarkets

      it "coal" do   
        cost Coal markets `shouldEqual` Just 1

      it "oil" do   
        cost Oil markets `shouldEqual` Just 3

      it "biomass" do   
        cost BioMass markets `shouldEqual` Just 7  

      it "uranium" do   
        cost Uranium markets `shouldEqual` Just 14

    describe "inc" do 
      let markets = dec 24 Coal newResourceMarkets

      it "until capacity" do   
        available Coal (inc 1 Coal markets) `shouldEqual` 1
        available Coal (inc 3 Coal markets) `shouldEqual` 3
        available Coal (inc 6 Coal markets) `shouldEqual` 6
        available Coal (inc 9 Coal markets) `shouldEqual` 9
        available Coal (inc 12 Coal markets) `shouldEqual` 12
        available Coal (inc 15 Coal markets) `shouldEqual` 15
        available Coal (inc 18 Coal markets) `shouldEqual` 18
        available Coal (inc 21 Coal markets) `shouldEqual` 21
        available Coal (inc 24 Coal markets) `shouldEqual` 24

      it "more than capacity" do   
        available Coal (inc 25 Coal markets) `shouldEqual` 24

    describe "dec" do 
      let markets = newResourceMarkets

      it "available" do   
        available Coal (dec 1 Coal markets) `shouldEqual` 23
        available Coal (dec 3 Coal markets) `shouldEqual` 21
        available Coal (dec 6 Coal markets) `shouldEqual` 18
        available Coal (dec 9 Coal markets) `shouldEqual` 15
        available Coal (dec 12 Coal markets) `shouldEqual` 12
        available Coal (dec 15 Coal markets) `shouldEqual` 9
        available Coal (dec 18 Coal markets) `shouldEqual` 6
        available Coal (dec 21 Coal markets) `shouldEqual` 3
        available Coal (dec 24 Coal markets) `shouldEqual` 0

      it "more than available" do   
        available Coal (dec 25 Coal markets) `shouldEqual` 0