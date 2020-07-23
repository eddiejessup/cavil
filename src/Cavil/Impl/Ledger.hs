{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Impl.Ledger where

import Cavil.Api.Ledger
import Cavil.Event.Common
import Cavil.Event.Ledger
import Cavil.Hashing
import qualified Data.Binary as B
import Data.Generics.Product.Typed
import Data.Generics.Sum (AsType, injectTyped)
import qualified Database.PostgreSQL.Simple as PG
import Protolude hiding ((%))

aggIdFromLedgerLabel :: LedgerLabel -> AggregateID
aggIdFromLedgerLabel ledgerLabel =
  AggregateID $
    uuidFromArbitraryByteString aggIdSalt $
      B.encode $
        getTyped @Text ledgerLabel
  where
    aggIdSalt = "Ym1JF76c5AEz"

fetchInitialLedgerAggByLabel ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState LedgerAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
  LedgerLabel ->
  m (Maybe LedgerAggregate, AggregateValidatedToken)
fetchInitialLedgerAggByLabel ledgerLabel =
  getAggregate (AggregateBeforeRequest (aggIdFromLedgerLabel ledgerLabel)) (Proxy @LedgerEvent)

fetchCreatedLedgerAggByLabel ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState LedgerAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
  LedgerLabel ->
  m (LedgerAggregate, AggregateValidatedToken)
fetchCreatedLedgerAggByLabel ledgerLabel =
  fetchInitialLedgerAggByLabel ledgerLabel >>= \case
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
  m ()
createLedger ledgerLabel = do
  (agg, valAggId) <- fetchInitialLedgerAggByLabel ledgerLabel
  let newEvts = [LedgerCreated (LedgerCreatedEvent ledgerLabel)]
  void $ insertEventsValidated valAggId agg newEvts

summariseLedger ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState LedgerAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
  LedgerLabel ->
  m LedgerSummary
summariseLedger ledgerLabel = do
  (agg, _) <- fetchCreatedLedgerAggByLabel ledgerLabel

  pure $
    LedgerSummary
      { label = getTyped @LedgerLabel agg,
        records = toRecordSummary
          <$> sortBy cmpRecordTime (getField @"records" agg)
      }
  where
    cmpRecordTime ::
      RecordAggregate ->
      RecordAggregate ->
      Ordering
    cmpRecordTime a b = getField @"recordTime" a `compare` getField @"recordTime" b

    toRecordSummary :: RecordAggregate -> RecordSummary
    toRecordSummary recAgg =
      RecordSummary
        { recordTime = getField @"recordTime" recAgg,
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
  LedgerLabel ->
  RecordTime ->
  RecordBody ->
  m ()
writeRecord ledgerLabel recTime recBody = do
  (agg, valAggId) <- fetchCreatedLedgerAggByLabel ledgerLabel
  let newEvts = [RecordWritten (RecordWrittenEvent recTime recBody)]
  void $ insertEventsValidated valAggId (Just agg) newEvts

getAllLedgerLabels ::
  (MonadIO m, MonadReader r m, HasType PG.Connection r) =>
  m [LedgerLabel]
getAllLedgerLabels =
  ledgerLabelsFromEvents <$> getAllEvents
