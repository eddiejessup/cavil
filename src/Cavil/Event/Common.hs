{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Event.Common where

import Data.Generics.Product.Typed
import Data.Generics.Sum (AsType, injectTyped)
import qualified Data.List as List
import Data.UUID (UUID)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple.ToField as PG
import Optics
import Protolude hiding (to, (%))

foldAllEventsIntoAggregate :: (MonadError e m, AsType (AggregateErrorWithState (AggErr evt)) e, CavilEvent evt) => AggregateState -> [evt] -> m (Maybe (EvtAggregate evt))
foldAllEventsIntoAggregate aggState = foldIncrementalEventsIntoAggregate aggState Nothing

foldIncrementalEventsIntoAggregate :: (MonadError e m, AsType (AggregateErrorWithState (AggErr evt)) e, CavilEvent evt) => AggregateState -> Maybe (EvtAggregate evt) -> [evt] -> m (Maybe (EvtAggregate evt))
foldIncrementalEventsIntoAggregate aggState = foldM (foldEvt aggState)

class (PG.ToField evt, PG.FromField evt) => CavilEvent evt where
  type EvtAggregate evt

  type AggErr evt

  eventType :: Proxy evt -> Text

  foldEvt :: (MonadError e m, AsType (AggregateErrorWithState (AggErr evt)) e) => AggregateState -> Maybe (EvtAggregate evt) -> evt -> m (Maybe (EvtAggregate evt))

getAggregate ::
  forall e r m evt.
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState (AggErr evt)) e, MonadReader r m, HasType PG.Connection r, CavilEvent evt) =>
  AggregateState ->
  Proxy evt ->
  m (Maybe (EvtAggregate evt), AggregateValidatedToken)
getAggregate aggState _pxy = do
  let aggId = aggIdFromState aggState
  pgConn <- gview (typed @PG.Connection)
  evts <-
    liftIO $
      PG.query @_ @(PG.Only evt)
        pgConn
        [sql|
            SELECT
              data
            FROM
              event
            WHERE
              event_type = ?
              and aggregate_id = ?
            ORDER BY
              sequence_nr ASC
          |]
        ("case" :: Text, aggId)
  agg <- foldAllEventsIntoAggregate aggState (PG.fromOnly <$> evts)
  pure (agg, AggregateValidatedToken aggId)

getAllEvents ::
  forall r m evt.
  (MonadIO m, MonadReader r m, HasType PG.Connection r, PG.FromField evt) =>
  m [evt]
getAllEvents = do
  pgConn <- gview (typed @PG.Connection)
  evts <-
    liftIO $
      PG.query @_ @(PG.Only evt)
        pgConn
        [sql|
          SELECT
            data
          FROM
            event
          WHERE
            event_type = ?
          ORDER BY
            sequence_nr ASC
        |]
        (PG.Only @Text "case")
  pure $ PG.fromOnly <$> evts

insertEventsValidated ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState (AggErr evt)) e, AsType WriteError e, MonadReader r m, HasType PG.Connection r, CavilEvent evt) =>
  AggregateValidatedToken ->
  Maybe (EvtAggregate evt) ->
  [evt] ->
  m (Maybe (EvtAggregate evt))
insertEventsValidated valAggId curAgg evts = do
  -- Check inserting event should go okay.
  newAgg <- foldIncrementalEventsIntoAggregate (AggregateDuringRequest valAggId) curAgg evts
  insertEvents valAggId evts
  pure newAgg

insertEvents ::
  (MonadIO m, MonadError e m, AsType WriteError e, MonadReader r m, HasType PG.Connection r, PG.ToField evt) =>
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
          [sql| INSERT INTO event (event_type, aggregate_id, data) VALUES (?, ?, ?)|]
          ( List.zip3
              (repeat @Text "case")
              (repeat (validatedAggId valAggId))
              evts
          )

  when (nrRowsAffected /= length evts) $
    throwError $
      injectTyped $
        InsertedUnexpectedNrRows (fromIntegral nrRowsAffected)

data AggregateState
  = AggregateBeforeRequest AggregateID
  | AggregateDuringRequest AggregateValidatedToken
  deriving stock (Generic)

newtype AggregateID = AggregateID {aggIdUUID :: UUID}
  deriving stock (Generic, Show)
  deriving newtype (PG.ToField, PG.FromField)

aggIdFromState :: AggregateState -> AggregateID
aggIdFromState = \case
  AggregateBeforeRequest aggId -> aggId
  AggregateDuringRequest valAggId -> validatedAggId valAggId

newtype AggregateValidatedToken = AggregateValidatedToken {validatedAggId :: AggregateID}

data AggregateErrorWithState e
  = AggregateErrorWithState AggregateState e
  deriving stock (Generic)

data WriteError
  = InsertedUnexpectedNrRows Int
  deriving stock (Generic, Show)
