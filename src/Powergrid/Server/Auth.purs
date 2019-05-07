module Powergrid.Server.Auth where

import Data.String
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either(..), note, either)
import Data.Maybe (Maybe(..), maybe)
import Database.PostgreSQL (withConnection)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log, logShow)
import Effect.Exception (Error(..), error)
import Effect.Unsafe (unsafePerformEffect)
import Node.Buffer (fromString, toString)
import Node.Encoding (Encoding(..))
import Node.Express.Handler (Handler, HandlerM, next)
import Node.Express.Request (getRequestHeader, getUserData, setUserData)
import Node.Express.Response (setStatus, end)
import Powergrid.Server.Handler (AppContext, throwIfNothing, throwIfNothingM, throwIfLeft, throwIfLeftM)
import Powergrid.Server.User (User(..), findUserByUsername)

logDebug :: forall m. MonadEffect m => String -> m Unit
logDebug msg = liftEffect $ log msg

authenticationHandler :: AppContext -> Handler
authenticationHandler {db} = do
  authorization <- throwIfNothingM "Authorization required" $ getRequestHeader "Authorization"
  logDebug $ "Authorization header: " <> authorization  
  UsernamePassword credentials <- throwIfLeft $ parseBasicAuth authorization
  User user <- liftAff $ throwIfNothingM "User not found" $ withConnection db $ findUserByUsername credentials.username
  -- TODO Check password
  setUserId user.id
  next

unauthorized :: Handler
unauthorized = do
  setStatus 401
  end

decodeBase64 :: String -> String
decodeBase64 s = unsafePerformEffect do
  buffer <- fromString s Base64
  toString UTF8 buffer

newtype UsernamePassword = UsernamePassword { username :: String, password :: String }

parseUsernamePassword :: String -> Either String UsernamePassword
parseUsernamePassword s =
  let pos = note "Must contain :" $ indexOf (Pattern ":") s
      parts = (\i -> splitAt i s) <$> pos
  in (\{before, after} -> UsernamePassword { username: before, password: after }) <$> parts

parseBasicAuth :: String -> Either String UsernamePassword
parseBasicAuth s = 
  let encoded = note "Invalid basic auth header" (stripPrefix (Pattern "Basic ") s)
      credentials = decodeBase64 <$> encoded
  in credentials >>= parseUsernamePassword

getUserId :: HandlerM String
getUserId = throwIfNothingM "User not authenticated" $ getUserData "userId"

setUserId :: String -> Handler
setUserId = setUserData "userId"
 