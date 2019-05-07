module Test.Spec.ResourceMarketSpec(resourceMarketSpec) where

import Prelude

import Data.Maybe (Maybe(..))
import ResourceMarket (available, capacity, cost, dec, inc, newDefaultResourceMarket, newUraniumResourceMarket)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

resourceMarketSpec :: Spec Unit
resourceMarketSpec = do

  describe "ResourceMarket" do

    describe "newDefaultResourceMarket" do 
      let market = inc 24 newDefaultResourceMarket

      it "available" do   
        available market `shouldEqual` 24

      it "capacity" do   
        capacity market `shouldEqual` 24

    describe "newUraniumResourceMarket" do 
      let market = inc 4 newUraniumResourceMarket

      it "available" do   
        available market `shouldEqual` 4

      it "capacity" do   
        capacity market `shouldEqual` 12

    describe "cost" do 
      it "default" do   
        let market = inc 24 newDefaultResourceMarket
        cost market 1 `shouldEqual` Just 1
        cost market 3 `shouldEqual` Just 3
        cost market 4 `shouldEqual` Just 5
        cost market 6 `shouldEqual` Just 9
        cost market 24 `shouldEqual` Just 108

      it "not enough available" do
        let market = inc 23 newDefaultResourceMarket
        cost market 24 `shouldEqual` Nothing

      it "uranium" do   
        let market = inc 12 newUraniumResourceMarket
        cost market 1 `shouldEqual` Just 1
        cost market 2 `shouldEqual` Just 3
        cost market 4 `shouldEqual` Just 10
        cost market 8 `shouldEqual` Just 36
        cost market 9 `shouldEqual` Just 46
        cost market 12 `shouldEqual` Just 88
        
    describe "inc" do 
      let market = newDefaultResourceMarket

      it "until capacity" do   
        cost (inc 1 market) 1 `shouldEqual` Just 8
        cost (inc 3 market) 1 `shouldEqual` Just 8
        cost (inc 6 market) 1 `shouldEqual` Just 7
        cost (inc 9 market) 1 `shouldEqual` Just 6
        cost (inc 12 market) 1 `shouldEqual` Just 5
        cost (inc 15 market) 1 `shouldEqual` Just 4
        cost (inc 18 market) 1 `shouldEqual` Just 3
        cost (inc 21 market) 1 `shouldEqual` Just 2
        cost (inc 24 market) 1 `shouldEqual` Just 1

      it "more than capacity" do   
        available (inc 25 market) `shouldEqual` 24

    describe "dec" do 
      let market = inc 24 newDefaultResourceMarket

      it "available" do   
        cost (dec 1 market) 1 `shouldEqual` Just 1
        cost (dec 3 market) 1 `shouldEqual` Just 2
        cost (dec 6 market) 1 `shouldEqual` Just 3
        cost (dec 9 market) 1 `shouldEqual` Just 4
        cost (dec 12 market) 1 `shouldEqual` Just 5
        cost (dec 15 market) 1 `shouldEqual` Just 6
        cost (dec 18 market) 1 `shouldEqual` Just 7
        cost (dec 21 market) 1 `shouldEqual` Just 8
        cost (dec 24 market) 1 `shouldEqual` Nothing

      it "more than available" do   
        available (dec 25 market) `shouldEqual` 0
