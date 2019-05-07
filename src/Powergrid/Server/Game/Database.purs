module Powergrid.Server.Game.Database where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.UUID (genUUID)
import Database.PostgreSQL (class FromSQLRow, class ToSQLRow, Connection, Query(..), query, execute, fromSQLValue, toSQLValue)
import Database.PostgreSQL.Row (Row1(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Now (now)

newtype Game = Game { 
    id :: String, 
    created :: Instant,
    hostUserId :: String,
    otherUserId :: String
  }

newGame :: String -> String -> Effect Game 
newGame hostUserId otherUserId = do
  id <- show <$> genUUID
  created <- now
  pure $ Game { id, created, hostUserId, otherUserId }

instance gameFromSQLRow :: FromSQLRow Game where
  fromSQLRow [id, created, hostUserId, otherUserId] =
    Game <$> ({ id: _, created: _, hostUserId: _, otherUserId: _ } 
         <$> fromSQLValue id
         <*> fromSQLValue created
         <*> fromSQLValue hostUserId
         <*> fromSQLValue otherUserId)
  fromSQLRow xs = Left $ "Invalid game row"

instance gameToSQLRow :: ToSQLRow Game where
  toSQLRow (Game { id, created, hostUserId, otherUserId }) = [
      toSQLValue id, 
      toSQLValue created, 
      toSQLValue hostUserId, 
      toSQLValue otherUserId
    ]    

queryGames :: String -> Connection -> Aff (Array Game)
queryGames userId conn = query conn (Query "SELECT id, created FROM game WHERE host_user_id=$1 OR other_user_id=$1") (Row1 userId)

insertGame :: Game -> Connection -> Aff Unit
insertGame game conn = execute conn (Query "INSERT INTO game (id, created, host_user_id, other_user_id) VALUES ($1, $2, $3, $4)") game
