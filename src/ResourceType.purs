module ResourceType (ResourceType(..)) where

import Prelude

data ResourceType = Coal | Oil | BioMass | Uranium | Wind

instance showResourceType :: Show ResourceType where
  show Coal = "Coal"
  show Oil = "Oil"
  show BioMass = "BioMass"
  show Uranium = "Uranium"            
  show Wind = "Wind"            

derive instance eqResourceType :: Eq ResourceType

derive instance ordResourceType :: Ord ResourceType
