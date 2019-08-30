module Powergrid.Server.Handler where

import Database.PostgreSQL (Pool)

type AppContext = { db :: Pool }
