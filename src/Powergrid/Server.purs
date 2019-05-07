module Powergrid.Server where

import Prelude

import Data.Maybe (Maybe(..))
import Database.PostgreSQL (PoolConfiguration, newPool)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Express.App (App, listenHttp, use)
import Powergrid.Server.Auth (authenticationHandler)
import Powergrid.Server.Game.Handler (gamesHandler)
import Powergrid.Server.Handler (AppContext)

app :: AppContext -> App
app ctx = do
  use $ authenticationHandler ctx
  gamesHandler ctx

databaseConfig :: PoolConfiguration
databaseConfig = { database: "powergrid"
      , host: Just "localhost"
      , idleTimeoutMillis: Nothing
      , max: Nothing
      , password: Just "secret"
      , port: Nothing
      , user: Just "powergrid"
      }

runServer :: Effect Unit
runServer = launchAff_ do
  db <- newPool databaseConfig
  liftEffect $ listenHttp (app { db }) 8080 \_ -> log "Listening..."