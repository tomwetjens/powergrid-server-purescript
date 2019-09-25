module Powergrid.Spec.GameSpec(gameSpec) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, try)
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.List (List(..), head, singleton)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map (empty) as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Set (empty, fromFoldable, singleton) as Set
import Effect.Class (liftEffect)
import Effect.Exception (Error, message)
import Node.Stream (onFinish)
import Powergrid.Game (Auction(..), Game(..), Phase(..), startAuction, startGame)
import Powergrid.Map.Germany (germany)
import Powergrid.Player (Player(..), newPlayer)
import Powergrid.PowerPlant (PowerPlant(..), cost)
import Powergrid.PowerPlantDeck (PowerPlantDeck(..))
import Powergrid.PowerPlantMarket (PowerPlantMarket(..), newPowerPlantMarket, actual)
import Powergrid.ResourceMarkets (newResourceMarkets)
import Powergrid.ResourceType (ResourceType(..))
import Powergrid.Util.Error (throwIfNothing)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)

initialGame :: forall m. MonadThrow Error m => m Game
initialGame = 
  let 
    alice = newPlayer "alice"
    bob = newPlayer "bob"
    players = NonEmptyList (NonEmpty alice (singleton bob))
    powerPlantMarket = newPowerPlantMarket $ PowerPlantDeck Nil
  in do
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
        initial <- initialGame
        let (Game game) = initial
        let (PowerPlantMarket powerPlantMarket) = game.powerPlantMarket
        powerPlant <- throwIfNothing "no future" $ head powerPlantMarket.future
        result <- try $ startAuction powerPlant (cost powerPlant) Nothing initial
        case result of
          Left err -> message err `shouldEqual` "PowerPlantNotInActual"
          Right _ -> fail("error expected")

      it "should throw error if must replace but not replaces was given" do
        (Game initial) <- initialGame
        let owned = Set.fromFoldable [ 
            (PowerPlant { cost: 20, types: Set.singleton Coal, requires: 3, powers: 5 }),
            (PowerPlant { cost: 25, types: Set.singleton Coal, requires: 2, powers: 5 }),
            (PowerPlant { cost: 31, types: Set.singleton Coal, requires: 3, powers: 6 }),
            (PowerPlant { cost: 36, types: Set.singleton Coal, requires: 3, powers: 7 })
          ]
        let alice = Player { name: "alice", balance: 50, powerPlants: owned, resources: Map.empty }
        let bob = Player { name: "bob", balance: 50, powerPlants: Set.empty, resources: Map.empty }
        let players = NonEmptyList (NonEmpty alice (singleton bob))
        let wrong = Game $ initial { players = players, phase = AuctionPhase { auction: Nothing, auctioningPlayers: players, closedAuctions: Set.empty } }
        let (PowerPlantMarket powerPlantMarket) = initial.powerPlantMarket
        powerPlant <- throwIfNothing "no actual" $ head powerPlantMarket.actual
        result <- try $ startAuction powerPlant 3 Nothing wrong
        case result of
          Left err -> message err `shouldEqual` "MustReplacePowerPlant"
          Right _ -> fail("error expected")

      it "should throw error if replaces is not owned by player" do
        (Game initial) <- initialGame
        let owned = Set.fromFoldable [ 
            (PowerPlant { cost: 20, types: Set.singleton Coal, requires: 3, powers: 5 }),
            (PowerPlant { cost: 25, types: Set.singleton Coal, requires: 2, powers: 5 }),
            (PowerPlant { cost: 31, types: Set.singleton Coal, requires: 3, powers: 6 }),
            (PowerPlant { cost: 36, types: Set.singleton Coal, requires: 3, powers: 7 })
          ]
        let alice = Player { name: "alice", balance: 50, powerPlants: owned, resources: Map.empty }
        let bob = Player { name: "bob", balance: 50, powerPlants: Set.empty, resources: Map.empty }
        let players = NonEmptyList (NonEmpty alice (singleton bob))
        let wrong = Game $ initial { players = players, phase = AuctionPhase { auction: Nothing, auctioningPlayers: players, closedAuctions: Set.empty } }
        let (PowerPlantMarket powerPlantMarket) = initial.powerPlantMarket
        powerPlant <- throwIfNothing "no actual" $ head powerPlantMarket.actual
        result <- try $ startAuction powerPlant 3 (Just powerPlant) wrong
        case result of
          Left err -> message err `shouldEqual` "NotPlayerPowerPlant"
          Right _ -> fail("error expected")

      it "should throw error if not enough balance" do
        (Game initial) <- initialGame
        let alice = Player { name: "alice", balance: 2, powerPlants: Set.empty, resources: Map.empty }
        let bob = Player { name: "bob", balance: 50, powerPlants: Set.empty, resources: Map.empty }
        let players = NonEmptyList (NonEmpty alice (singleton bob))
        let wrong = Game $ initial { players = players, phase = AuctionPhase { auction: Nothing, auctioningPlayers: players, closedAuctions: Set.empty } }
        let (PowerPlantMarket powerPlantMarket) = initial.powerPlantMarket
        powerPlant <- throwIfNothing "no actual" $ head powerPlantMarket.actual
        result <- try $ startAuction powerPlant 3 Nothing wrong
        case result of
          Left err -> message err `shouldEqual` "NotEnoughBalance"
          Right _ -> fail("error expected")

      it "should throw error if bid too low" do
        initial <- initialGame
        let (Game game) = initial
        let (PowerPlantMarket powerPlantMarket) = game.powerPlantMarket
        powerPlant <- throwIfNothing "no actual" $ head powerPlantMarket.actual
        result <- try $ startAuction powerPlant 2 Nothing initial
        case result of
          Left err -> message err `shouldEqual` "BidTooLow"
          Right _ -> fail("error expected")

      it "should start auction with replacement" do
        (Game initial) <- initialGame
        let replaces = (PowerPlant { cost: 20, types: Set.singleton Coal, requires: 3, powers: 5 })
        let owned = Set.fromFoldable [ 
            replaces,
            (PowerPlant { cost: 25, types: Set.singleton Coal, requires: 2, powers: 5 }),
            (PowerPlant { cost: 31, types: Set.singleton Coal, requires: 3, powers: 6 }),
            (PowerPlant { cost: 36, types: Set.singleton Coal, requires: 3, powers: 7 })
          ]
        let alice = Player { name: "alice", balance: 50, powerPlants: owned, resources: Map.empty }
        let bob = Player { name: "bob", balance: 50, powerPlants: Set.empty, resources: Map.empty }
        let players = NonEmptyList (NonEmpty alice (singleton bob))
        let game = Game $ initial { players = players, phase = AuctionPhase { auction: Nothing, auctioningPlayers: players, closedAuctions: Set.empty } }
        let (PowerPlantMarket powerPlantMarket) = initial.powerPlantMarket
        powerPlant <- throwIfNothing "no actual" $ head powerPlantMarket.actual
        (Game result) <- startAuction powerPlant 3 (Just replaces) game
        case result.phase of
          (AuctionPhase auctionPhase) ->
            case auctionPhase.auction of
              Just (Auction auction) -> do
                auction.replaces `shouldEqual` Just replaces
              Nothing -> fail("no auction")  
          phase -> fail("unexpected phase: " <> show phase)  