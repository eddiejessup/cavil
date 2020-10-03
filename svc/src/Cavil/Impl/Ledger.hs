module Cavil.Impl.Ledger where

import Cavil.Api.Ledger
import Cavil.Api.Ledger.Var
import Cavil.Event.Common
import Cavil.Event.Ledger
import Data.Generics.Product.Typed
import Data.Generics.Sum (AsType, injectTyped)
import Data.List qualified as List
import Data.Map.Strict (lookup, traverseWithKey)
import Data.Time qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import Database.PostgreSQL.Simple qualified as PG
import Protolude hiding ((%))

pickVariant :: VariantList -> FieldId -> VariantSelection
pickVariant variants entryId =
  let (w1, w2, w3, w4) = UUID.toWords $ getTyped @UUID entryId
      nonModuloInt = abs $ sum $ fromIntegral @Word32 @Int <$> [w1, w2, w3, w4]
      moduloInt = nonModuloInt `mod` length (getTyped @[Variant] variants)
   in case selectVariantByIdx variants moduloInt of
        Nothing -> panic "impossible"
        Just v -> v

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

parseLedgerSchema :: MonadIO m => LedgerSchema -> m InternalLedgerSchema
parseLedgerSchema = mapM withFieldId
  where
    withFieldId variants = do
      fieldId <- FieldId <$> liftIO UUID.V4.nextRandom
      pure (variants, fieldId)

createLedger ::
  ( MonadIO m,
    MonadError e m,
    AsType (AggregateErrorWithState LedgerAggregateError) e,
    AsType WriteError e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  LedgerLabel ->
  LedgerSchema ->
  m LedgerId
createLedger ledgerLabel schema = do
  evTime <- liftIO T.getCurrentTime
  ledgerId <- LedgerId <$> liftIO UUID.V4.nextRandom
  (agg, valAggId) <- fetchInitialLedgerAggById ledgerId
  internalSchema <- parseLedgerSchema schema
  let newEvts = [LedgerCreated (LedgerCreatedEvent ledgerLabel internalSchema)]
  void $ insertEventsValidated valAggId agg evTime newEvts
  pure ledgerId

summariseLedger ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState LedgerAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
  LedgerId ->
  m LedgerSummary
summariseLedger ledgerId = do
  (agg, _) <- fetchCreatedLedgerAggById ledgerId

  let entries =
        sortBy cmpEntryCreationTime (getField @"entries" agg)
          <&> \(id, (createTime, entryAgg)) -> toEntrySummary id createTime entryAgg

  pure $
    LedgerSummary
      { id = ledgerId,
        nextEntryId = nextEntryIdFromAgg ledgerId agg,
        label = getTyped @LedgerLabel agg,
        schema = asPublicSchema (getTyped @InternalLedgerSchema agg),
        entries
      }
  where
    cmpEntryCreationTime ::
      (EntryId, (T.UTCTime, EntryAggregate)) ->
      (EntryId, (T.UTCTime, EntryAggregate)) ->
      Ordering
    cmpEntryCreationTime (_, (tA, _)) (_, (tB, _)) = tA `compare` tB

    asPublicSchema = fmap fst

toEntrySummary :: EntryId -> T.UTCTime -> EntryAggregate -> EntrySummary
toEntrySummary id entryCreateTime entryAgg =
  EntrySummary
    { id,
      creationTime = entryCreateTime,
      body = getField @"entry" entryAgg,
      isValid = case getTyped @EntryValidity entryAgg of
        EntryIsValid -> True
        EntryIsNotValid _ -> False,
      invalidationReason = case getTyped @EntryValidity entryAgg of
        EntryIsValid -> Nothing
        EntryIsNotValid reason -> Just reason
    }

recordEntryRandom ::
  ( MonadIO m,
    MonadError e m,
    AsType (AggregateErrorWithState LedgerAggregateError) e,
    AsType WriteError e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  LedgerId ->
  EntryId ->
  RecordEntryRequest ->
  m RecordEntryResponse
recordEntryRandom ledgerId reqEntryId reqBody = do
  (agg, valAggId) <- fetchCreatedLedgerAggById ledgerId
  case List.lookup reqEntryId (getField @"entries" agg) of
    Nothing -> do
      nowTime <- liftIO T.getCurrentTime
      let schema = getTyped @InternalLedgerSchema agg
      entry <- flip traverseWithKey reqBody $ \fieldLabel fieldValSpec -> do
        (variants, fieldId) <- case lookup fieldLabel schema of
          Nothing -> throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) (undefined :: LedgerAggregateError)
          Just v -> pure v
        case fieldValSpec of
          RandomFieldValSpec ->
            pure FieldVal {v = pickVariant variants fieldId, entryTime = nowTime}
          ManualFieldValSpec ev ->
            case collectMaybeFieldVal (selectVariantByLabel variants) ev of
              Nothing ->
                throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) NoSuchVariant
              Just a ->
                pure a
      let newEvts = [EntryMade (EntryMadeEvent reqEntryId entry)]
      void $ insertEventsValidated valAggId (Just agg) nowTime newEvts
      pure entry
    Just (_existingDecCreatedTime, existingDecAgg) ->
      pure $ getTyped @InternalEntry existingDecAgg

summariseEntry ::
  ( MonadIO m,
    MonadError e m,
    AsType (AggregateErrorWithState LedgerAggregateError) e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  LedgerId ->
  EntryId ->
  m EntrySummary
summariseEntry ledgerId reqEntryId = do
  (agg, valAggId) <- fetchCreatedLedgerAggById ledgerId
  case List.lookup reqEntryId (getField @"entries" agg) of
    Nothing ->
      throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) NoSuchEntry
    Just (decCreateTime, entryAgg) ->
      pure $ toEntrySummary reqEntryId decCreateTime entryAgg

invalidateEntry ::
  ( MonadIO m,
    MonadError e m,
    AsType (AggregateErrorWithState LedgerAggregateError) e,
    AsType WriteError e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  LedgerId ->
  EntryId ->
  Text ->
  m ()
invalidateEntry ledgerId reqEntryId invalidateReason = do
  (agg, valAggId) <- fetchCreatedLedgerAggById ledgerId
  let newEvts = [EntryInvalidated (EntryInvalidatedEvent reqEntryId invalidateReason)]
  evTime <- liftIO T.getCurrentTime
  void $ insertEventsValidated valAggId (Just agg) evTime newEvts

getAllLedgerIds ::
  (MonadIO m, MonadReader r m, HasType PG.Connection r) =>
  m [LedgerId]
getAllLedgerIds =
  fmap (LedgerId . unAggregateId) <$> getAllAggIds (Proxy @LedgerEvent)
