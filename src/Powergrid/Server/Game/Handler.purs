module Powergrid.Server.Game.Handler where

import Prelude

import Database.PostgreSQL (withConnection)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Node.Express.App (App, get, post)
import Node.Express.Handler (Handler)
import Node.Express.Response (sendJson)
import Powergrid.Server.Auth (getUserId)
import Powergrid.Server.Game.Database (queryGames, insertGame, newGame)
import Powergrid.Server.Handler (AppContext)

gamesHandler :: AppContext -> App
gamesHandler ctx = do
  get "/games" $ getGamesHandler ctx
  post "/games" $ createGameHandler ctx

getGamesHandler :: AppContext -> Handler
getGamesHandler ctx = do
  userId <- getUserId
  games <- liftAff $ withConnection ctx.db $ queryGames userId
  sendJson games

createGameHandler :: AppContext -> Handler
createGameHandler ctx = do
  userId <- getUserId
  let otherUserId = "bar"
  game <- liftEffect $ newGame userId otherUserId
  liftAff $ withConnection ctx.db (insertGame game)
  sendJson game
