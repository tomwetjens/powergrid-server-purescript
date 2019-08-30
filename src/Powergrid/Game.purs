module Powergrid.Game where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either(..))
import Data.Foldable (length, elem)
import Data.List (List(..), (:), fromFoldable, snoc)
import Data.List.NonEmpty (NonEmptyList(..), fromList, head)
import Data.Map (Map)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..), (:|), fromNonEmpty)
import Data.Set (Set)
import Data.Set (empty, member) as Set
import Data.Traversable (class Traversable)
import Effect (Effect)
import Effect.Exception (Error, error)
import Powergrid.NetworkMap (NetworkMap)
import Powergrid.Player (Player(..), newPlayer)
import Powergrid.PowerPlant (PowerPlant, cost)
import Powergrid.PowerPlantDeck (newPowerPlantDeck)
import Powergrid.PowerPlantMarket (PowerPlantMarket, newPowerPlantMarket, actual)
import Powergrid.ResourceMarkets (ResourceMarkets, newResourceMarkets)
import Powergrid.Util.Error (throwIfNothing)
import Powergrid.Util.Shuffle (shuffle)

data Auction
  = Auction
    { biddingPlayers :: NonEmptyList Player
    , powerPlant :: PowerPlant
    , bid :: Int
    , replaces :: Maybe PowerPlant
    }

instance showAuction :: Show Auction where
  show (Auction auction) =
    "Auction:"
      <> " powerPlant="
      <> show auction.powerPlant
      <> " bid="
      <> show auction.bid
      <> " biddingPlayers="
      <> show auction.biddingPlayers
      <> " replaces="
      <> show auction.replaces

derive instance eqAuction :: Eq Auction

data Phase
  = AuctionPhase
    { auction :: Maybe Auction
    , auctioningPlayers :: NonEmptyList Player
    , closedAuctions :: Set Auction
    }
  | BuyResourcesPhase
  | BuildPhase
  | BureaucracyPhase

startAuctionPhase :: NonEmptyList Player -> Phase
startAuctionPhase auctioningPlayers =
  AuctionPhase
    { auction: Nothing
    , auctioningPlayers
    , closedAuctions: Set.empty
    }

instance showPhase :: Show Phase where
  show (AuctionPhase auctionPhase) =
    "AuctionPhase:"
      <> " auction="
      <> show auctionPhase.auction
      <> " auctioningPlayers="
      <> show auctionPhase.auctioningPlayers
      <> " closedAuctions="
      <> show auctionPhase.closedAuctions
  show BuyResourcesPhase = "BuyResourcesPhase"
  show BuildPhase = "BuildPhase"
  show BureaucracyPhase = "BureaucracyPhase"

data Game
  = Game
    { map :: NetworkMap
    , step :: Int
    , round :: Int
    , players :: NonEmptyList Player
    , phase :: Phase
    , powerPlantMarket :: PowerPlantMarket
    , resourceMarkets :: ResourceMarkets
    }

-- | Starts a new `Game` for the given `NetworkMap` and players.
startGame :: forall f. Traversable f => NetworkMap -> f String -> Effect Game
startGame map names = do
  deck <- newPowerPlantDeck $ length names
  shuffled <- shuffle $ newPlayer <$> fromFoldable names
  players <- throwIfNothing "NoPlayers" $ fromList $ shuffled
  pure
    $ Game
        { map
        , players
        , step: 1
        , round: 1
        , phase: startAuctionPhase players
        , powerPlantMarket: newPowerPlantMarket deck
        , resourceMarkets: newResourceMarkets
        }

-- | Starts an auction of a power plant.
startAuction :: forall m. MonadThrow Error m => PowerPlant -> Int -> Maybe PowerPlant -> Game -> m Game
startAuction powerPlant bid replaces (Game game@{ phase: AuctionPhase auctionPhase@{ auction: Nothing } }) = do
  when (bid < cost powerPlant) do
    throwError $ error "BidTooLow" 
  when (not elem powerPlant (actual game.powerPlantMarket)) do
    throwError $ error "PowerPlantNotInActual"            
  let (Player currentAuctioningPlayer) = head auctionPhase.auctioningPlayers
  when (bid > currentAuctioningPlayer.balance) do
    throwError $ error "NotEnoughBalance"            
  let mustReplacePowerPlant = length currentAuctioningPlayer.powerPlants >= maxOwnedPowerPlants (length game.players)
  when mustReplacePowerPlant do
    case replaces of
      Nothing -> throwError $ error "MustReplacePowerPlant"
      Just p -> when (not elem p currentAuctioningPlayer.powerPlants) do 
        throwError $ error "NotPlayerPowerPlant"    
  pure $ Game $ game { phase = newAuctionPhase }
  where
    biddingPlayers = rotate auctionPhase.auctioningPlayers
    newAuction = Auction { biddingPlayers, bid, powerPlant, replaces }
    newAuctionPhase = AuctionPhase $ auctionPhase { auction = Just newAuction }
startAuction _ _ _ (Game { phase: AuctionPhase _ }) = throwError $ error "AuctionInProgress"
startAuction _ _ _ (Game { phase: _ }) = throwError $ error "NotAuctionPhase"

placeBid :: forall m. MonadThrow Error m => Int -> Maybe PowerPlant -> Game -> m Game
placeBid bid replaces (Game game@{ phase: AuctionPhase auctionPhase@{ auction: Just (Auction auction@_) } }) = pure $ Game game { phase = newAuctionPhase }
  where
    newBiddingPlayers = rotate auctionPhase.auctioningPlayers
    newAuction = Auction $ auction { biddingPlayers = newBiddingPlayers, bid = bid, replaces = replaces }
    newAuctionPhase = AuctionPhase $ auctionPhase { auction = Just newAuction }
placeBid _ _ (Game { phase: AuctionPhase _ }) = throwError $ error "NoAuctionInProgress"
placeBid _ _ (Game { phase: _ }) = throwError $ error "NotAuctionPhase"

rotate :: forall a. NonEmptyList a -> NonEmptyList a
rotate (NonEmptyList (NonEmpty a (b : xb))) = NonEmptyList (NonEmpty b (snoc xb a))
rotate input@(NonEmptyList (NonEmpty _ Nil)) = input

maxOwnedPowerPlants :: Int -> Int
maxOwnedPowerPlants 2 = 4
maxOwnedPowerPlants _ = 3

step2StartsOnNumberOfCities :: Int -> Int
step2StartsOnNumberOfCities 2 = 10
step2StartsOnNumberOfCities 6 = 6
step2StartsOnNumberOfCities _ = 7

gameEndsOnNumberOfCities :: Int -> Int
gameEndsOnNumberOfCities 2 = 21
gameEndsOnNumberOfCities 5 = 15
gameEndsOnNumberOfCities 6 = 14
gameEndsOnNumberOfCities _ = 17