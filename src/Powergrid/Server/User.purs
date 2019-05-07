module Powergrid.Server.User where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe)
import Data.Either (Either(..))
import Database.PostgreSQL (class FromSQLRow, class ToSQLRow, Connection, Query(..), query, fromSQLValue, toSQLValue)
import Database.PostgreSQL.Row (Row1(..))
import Effect.Aff (Aff)

newtype User = User { 
    id :: String, 
    username :: String,
    password :: String
  }

instance userFromSQLRow :: FromSQLRow User where
  fromSQLRow [id, username, password] =
    User <$> ({ id: _, username: _, password: _ } 
         <$> fromSQLValue id
         <*> fromSQLValue username
         <*> fromSQLValue password)
  fromSQLRow xs = Left $ "Invalid user row"

instance userToSQLRow :: ToSQLRow User where
  toSQLRow (User u) = [
      toSQLValue u.id, 
      toSQLValue u.username, 
      toSQLValue u.password
    ]    

findUserByUsername :: String -> Connection -> Aff (Maybe User)
findUserByUsername username conn = do
  results <- query conn (Query "SELECT id, username, password FROM \"user\" WHERE username=$1") (Row1 username)
  pure $ head results
