module Powergrid.Spec.ResourceMarketsSpec(resourceMarketsSpec) where

import Prelude

import ResourceType (ResourceType(..))
import ResourceMarkets (newResourceMarkets, availableRT, capacityRT)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

resourceMarketsSpec :: Spec Unit
resourceMarketsSpec = do

  describe "ResourceMarkets" do
    let markets = newResourceMarkets

    describe "newResourceMarkets" do 
      it "available" do   
        availableRT Coal markets `shouldEqual` 24
        availableRT Oil markets `shouldEqual` 18
        availableRT BioMass markets `shouldEqual` 6
        availableRT Uranium markets `shouldEqual` 2

      it "capacity" do   
        capacityRT Coal markets `shouldEqual` 24
        capacityRT Oil markets `shouldEqual` 24
        capacityRT BioMass markets `shouldEqual` 24
        capacityRT Uranium markets `shouldEqual` 12
