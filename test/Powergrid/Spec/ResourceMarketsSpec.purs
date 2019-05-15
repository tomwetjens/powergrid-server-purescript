module Powergrid.Spec.ResourceMarketsSpec(resourceMarketsSpec) where

import Prelude

import Powergrid.ResourceType (ResourceType(..))
import Powergrid.ResourceMarkets (newResourceMarkets, available, capacity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

resourceMarketsSpec :: Spec Unit
resourceMarketsSpec = do

  describe "ResourceMarkets" do
    let markets = newResourceMarkets

    describe "newResourceMarkets" do 
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
