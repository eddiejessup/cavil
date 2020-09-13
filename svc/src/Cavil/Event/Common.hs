{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Event.Common where

import Data.Generics.Product.Typed
import Data.Generics.Sum (AsType, injectTyped)
import qualified Data.List as List
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import Optics
import Protolude hiding (to, (%))

foldAllEventsIntoAggregate :: (MonadError e m, AsType (AggregateErrorWithState (AggErr evt)) e, CavilEvent evt) => AggregateState -> [(evt, UTCTime)] -> m (Maybe (EvtAggregate evt))
foldAllEventsIntoAggregate aggState = foldIncrementalEventsIntoAggregate aggState Nothing

foldIncrementalEventsIntoAggregate :: (MonadError e m, AsType (AggregateErrorWithState (AggErr evt)) e, CavilEvent evt) => AggregateState -> Maybe (EvtAggregate evt) -> [(evt, UTCTime)] -> m (Maybe (EvtAggregate evt))
foldIncrementalEventsIntoAggregate aggState = foldM (foldEvt aggState)

class CavilEvent evt where
  type EvtAggregate evt

  type AggErr evt

  eventType :: Proxy evt -> Text

  foldEvt :: (MonadError e m, AsType (AggregateErrorWithState (AggErr evt)) e) => AggregateState -> Maybe (EvtAggregate evt) -> (evt, UTCTime) -> m (Maybe (EvtAggregate evt))

getAggregate ::
  forall e r m evt.
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState (AggErr evt)) e, MonadReader r m, HasType PG.Connection r, PG.FromField evt, CavilEvent evt) =>
  AggregateState ->
  Proxy evt ->
  m (Maybe (EvtAggregate evt), AggregateValidatedToken)
getAggregate aggState pxy = do
  let aggId = aggIdFromState aggState
  pgConn <- gview (typed @PG.Connection)
  evts <-
    liftIO $
      PG.query @_ @(evt, UTCTime)
        pgConn
        "\
        \ SELECT\
        \     data,\
        \     created\
        \ FROM\
        \    event\
        \ WHERE\
        \    event_type = ?\
        \    and aggregate_id = ?\
        \ ORDER BY\
        \    sequence_nr ASC\
        \"
        (eventType pxy, aggId)
  agg <- foldAllEventsIntoAggregate aggState evts
  pure (agg, AggregateValidatedToken aggId)

getAllAggIds ::
  forall r m evt.
  (MonadIO m, MonadReader r m, HasType PG.Connection r, CavilEvent evt) =>
  Proxy evt ->
  m [AggregateId]
getAllAggIds pxy = do
  pgConn <- gview (typed @PG.Connection)
  aggIds <-
    liftIO $
      PG.query @_ @(PG.Only AggregateId)
        pgConn
        "\
        \ SELECT\
        \   distinct aggregate_id\
        \ FROM\
        \   event\
        \ WHERE\
        \   event_type = ?\
        \ ORDER BY\
        \   aggregate_id ASC\
        \ "
        (PG.Only (eventType pxy))
  pure $ PG.fromOnly <$> aggIds

insertEventsValidated ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState (AggErr evt)) e, AsType WriteError e, MonadReader r m, HasType PG.Connection r, CavilEvent evt, PG.ToField evt) =>
  AggregateValidatedToken ->
  Maybe (EvtAggregate evt) ->
  UTCTime ->
  [evt] ->
  m (Maybe (EvtAggregate evt))
insertEventsValidated valAggId curAgg createTime evts = do
  -- Check inserting event should go okay.
  let evtsWithTime = (,createTime) <$> evts
  newAgg <- foldIncrementalEventsIntoAggregate (AggregateDuringRequest valAggId) curAgg evtsWithTime
  insertEvents valAggId evts
  pure newAgg

insertEvents ::
  forall e r m evt.
  (MonadIO m, MonadError e m, AsType WriteError e, MonadReader r m, HasType PG.Connection r, PG.ToField evt, CavilEvent evt) =>
  AggregateValidatedToken ->
  [evt] ->
  m ()
insertEvents valAggId evts = do
  pgConn <- gview (typed @PG.Connection)
  nrRowsAffected <-
    fmap (fromIntegral @Int64 @Int) $
      liftIO $
        PG.executeMany
          pgConn
          "INSERT INTO event (event_type, aggregate_id, data) VALUES (?, ?, ?)"
          ( List.zip3
              (repeat (eventType (Proxy @evt)))
              (repeat (validatedAggId valAggId))
              evts
          )

  when (nrRowsAffected /= length evts) $
    throwError $
      injectTyped $
        InsertedUnexpectedNrRows (fromIntegral nrRowsAffected)

data AggregateState
  = AggregateBeforeRequest AggregateId
  | AggregateDuringRequest AggregateValidatedToken
  deriving stock (Generic)

newtype AggregateId = AggregateId {unAggregateId :: UUID}
  deriving stock (Generic, Show)
  deriving newtype (PG.ToField, PG.FromField)

aggIdFromState :: AggregateState -> AggregateId
aggIdFromState = \case
  AggregateBeforeRequest aggId -> aggId
  AggregateDuringRequest valAggId -> validatedAggId valAggId

newtype AggregateValidatedToken = AggregateValidatedToken {validatedAggId :: AggregateId}

data AggregateErrorWithState e
  = AggregateErrorWithState AggregateState e
  deriving stock (Generic)

data WriteError
  = InsertedUnexpectedNrRows Int
  deriving stock (Generic, Show)
