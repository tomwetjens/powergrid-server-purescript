module Powergrid.Spec.GameSpec(gameSpec) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, catchError, throwError, try)
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.List (List(..), head, singleton)
import Data.List.NonEmpty (NonEmptyList(..), fromFoldable)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Set (empty) as Set
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, message)
import Powergrid.Game (Auction(..), Game(..), Phase(..), startAuction, startAuctionPhase, startGame)
import Powergrid.Map.Germany (germany)
import Powergrid.Player (Player(..), newPlayer)
import Powergrid.PowerPlantDeck (PowerPlantDeck(..))
import Powergrid.PowerPlantMarket (PowerPlantMarket(..), newPowerPlantMarket, actual)
import Powergrid.ResourceMarkets (newResourceMarkets)
import Powergrid.Util.Error (throwIfNothing)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)

initialGame :: forall m. MonadThrow Error m => m Game
initialGame = 
  let 
    alice = newPlayer "alice"
    bob = newPlayer "bob"
    powerPlantMarket = newPowerPlantMarket $ PowerPlantDeck Nil
  in do
    players <- throwIfNothing "NoPlayers" $ fromFoldable [alice, bob]
    powerPlant <- throwIfNothing "no actual" $ head $ actual powerPlantMarket
    pure $ Game {
      map: germany,
      players,
      step: 1,
      round: 1,
      phase: AuctionPhase {
        auction: Nothing,
        auctioningPlayers: players,
        closedAuctions: Set.empty
      },
      powerPlantMarket,
      resourceMarkets: newResourceMarkets
    }


gameSpec :: Spec Unit
gameSpec = do

  describe "Game" do

    describe "startGame" do
      it "2 players" do
        (Game game) <- liftEffect $ startGame germany ["alice", "bob"]
        game.step `shouldEqual` 1
        game.round `shouldEqual` 1
        length game.players `shouldEqual` 2
        case game.phase of
          (AuctionPhase auctionPhase) -> do
            auctionPhase.auction `shouldEqual` Nothing
            length auctionPhase.auctioningPlayers `shouldEqual` 2
            auctionPhase.closedAuctions `shouldEqual` Set.empty
          phase -> fail("unexpected phase: " <> show phase)  
        
    describe "startAuction" do
      it "should start auction" do
        initial <- initialGame
        let (Game game) = initial
        let (PowerPlantMarket powerPlantMarket) = game.powerPlantMarket
        powerPlant <- throwIfNothing "no actual" $ head powerPlantMarket.actual
        (Game result) <- startAuction powerPlant 3 Nothing initial
        case result.phase of
          (AuctionPhase auctionPhase) -> do
            (Auction auction) <- throwIfNothing "no auction" auctionPhase.auction  
            auction.bid `shouldEqual` 3
            auction.powerPlant `shouldEqual` powerPlant
            ((\(Player p) -> p.name) <$> auction.biddingPlayers) `shouldEqual` (NonEmptyList (NonEmpty "bob" (singleton "alice")))
            ((\(Player p) -> p.name) <$> auctionPhase.auctioningPlayers) `shouldEqual` (NonEmptyList (NonEmpty "alice" (singleton "bob")))
          phase -> fail("unexpected phase: " <> show phase)  

      it "should throw error if not auction phase" do
        (Game initial) <- initialGame
        let wrong = Game $ initial { phase = BuyResourcesPhase }
        let (PowerPlantMarket powerPlantMarket) = initial.powerPlantMarket
        powerPlant <- throwIfNothing "no actual" $ head powerPlantMarket.actual
        result <- try $ startAuction powerPlant 3 Nothing wrong
        case result of
          Left err -> message err `shouldEqual` "NotAuctionPhase"
          Right _ -> fail("error expected")

      it "should throw error if auction in progress" do
        (Game initial) <- initialGame
        let (PowerPlantMarket powerPlantMarket) = initial.powerPlantMarket
        powerPlant <- throwIfNothing "no actual" $ head powerPlantMarket.actual
        let wrong = Game $ initial { phase = AuctionPhase { auction: Just $ Auction { biddingPlayers: initial.players, bid: 3, powerPlant: powerPlant, replaces: Nothing }, auctioningPlayers: initial.players, closedAuctions: Set.empty } }
        result <- try $ startAuction powerPlant 3 Nothing wrong
        case result of
          Left err -> message err `shouldEqual` "AuctionInProgress"
          Right _ -> fail("error expected")

      it "should throw error if power plant not in actual" do
        fail("TODO")    

      it "should throw error if must replace but not replaces was given" do
        fail("TODO")      

      it "should throw error if replaces is not of player" do
        fail("TODO")      

      it "should throw error if not enough balance" do
       fail("TODO")        

      it "should throw error if bid too low" do
       fail("TODO")          