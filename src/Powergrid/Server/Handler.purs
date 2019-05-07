module Powergrid.Server.Handler where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either, either)
import Data.Maybe (Maybe, maybe')
import Database.PostgreSQL (Pool)
import Effect.Exception (error, Error)

type AppContext = { db :: Pool }

throwIfNothing :: forall a m. MonadThrow Error m => String -> Maybe a -> m a
throwIfNothing msg = maybe' (\_ -> throwError (error msg)) pure

throwIfNothingM :: forall a m. Bind m => MonadThrow Error m => String -> m (Maybe a) -> m a
throwIfNothingM msg m = m >>= throwIfNothing msg

throwIfLeft :: forall a m. MonadThrow Error m => Either String a -> m a
throwIfLeft = either (\msg -> throwError (error msg)) pure

throwIfLeftM :: forall a m. Bind m => MonadThrow Error m => m (Either String a) -> m a
throwIfLeftM m = m >>= throwIfLeft
