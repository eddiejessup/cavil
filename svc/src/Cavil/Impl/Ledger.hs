{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Impl.Ledger where

import Cavil.Api.Ledger
import Cavil.Event.Common
import Cavil.Event.Ledger
import Data.Generics.Product.Typed
import Data.Generics.Sum (AsType, injectTyped)
import qualified Data.UUID.V4 as UUID.V4
import qualified Database.PostgreSQL.Simple as PG
import Protolude hiding ((%))

fetchInitialLedgerAggById ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState LedgerAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
  LedgerId ->
  m (Maybe LedgerAggregate, AggregateValidatedToken)
fetchInitialLedgerAggById ledgerId =
  getAggregate (AggregateBeforeRequest (AggregateId (unLedgerId ledgerId))) (Proxy @LedgerEvent)

fetchCreatedLedgerAggById ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState LedgerAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
  LedgerId ->
  m (LedgerAggregate, AggregateValidatedToken)
fetchCreatedLedgerAggById ledgerId =
  fetchInitialLedgerAggById ledgerId >>= \case
    (Nothing, valAggId) ->
      throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) NoSuchLedger
    (Just agg, valAggId) ->
      pure (agg, valAggId)

createLedger ::
  ( MonadIO m,
    MonadError e m,
    AsType (AggregateErrorWithState LedgerAggregateError) e,
    AsType WriteError e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  LedgerLabel ->
  m LedgerId
createLedger ledgerLabel = do
  ledgerId <- LedgerId <$> liftIO UUID.V4.nextRandom
  (agg, valAggId) <- fetchInitialLedgerAggById ledgerId
  let newEvts = [LedgerCreated (LedgerCreatedEvent ledgerLabel)]
  void $ insertEventsValidated valAggId agg newEvts
  pure ledgerId

summariseLedger ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState LedgerAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
  LedgerId ->
  m LedgerSummary
summariseLedger ledgerId = do
  (agg, _) <- fetchCreatedLedgerAggById ledgerId
  pure
    LedgerSummary
      { id = ledgerId,
        label = getTyped @LedgerLabel agg,
        records =
          toRecordSummary
            <$> sortBy cmpRecordTime (getField @"records" agg)
      }
  where
    cmpRecordTime ::
      (RecordId, RecordAggregate) ->
      (RecordId, RecordAggregate) ->
      Ordering
    cmpRecordTime (_, a) (_, b) = getTyped @RecordTime a `compare` getTyped @RecordTime b

    toRecordSummary :: (RecordId, RecordAggregate) -> RecordSummary
    toRecordSummary (id, recAgg) =
      RecordSummary
        { id,
          recordTime = getTyped @RecordTime recAgg,
          body = getTyped @RecordBody recAgg
        }

writeRecord ::
  ( MonadIO m,
    MonadError e m,
    AsType (AggregateErrorWithState LedgerAggregateError) e,
    AsType WriteError e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  LedgerId ->
  RecordTime ->
  RecordBody ->
  m RecordId
writeRecord ledgerId recTime recBody = do
  recordId <- RecordId <$> liftIO UUID.V4.nextRandom
  (agg, valAggId) <- fetchCreatedLedgerAggById ledgerId
  let newEvts = [RecordWritten (RecordWrittenEvent recordId recTime recBody)]
  void $ insertEventsValidated valAggId (Just agg) newEvts
  pure recordId

getAllLedgerIds ::
  (MonadIO m, MonadReader r m, HasType PG.Connection r) =>
  m [LedgerId]
getAllLedgerIds =
  fmap (LedgerId . unAggregateId) <$> getAllAggIds (Proxy @LedgerEvent)
