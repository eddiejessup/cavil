module Cavil.Impl.Ledger where

import Cavil.Api.Ledger
import Cavil.Api.Ledger.Var
import Cavil.Event.Common
import Cavil.Event.Ledger
import Cavil.Hashing
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Binary qualified as B
import Data.ByteString.Lazy qualified as BS.L
import Data.Generics.Product.Typed
import Data.Generics.Sum (AsType, injectTyped)
import Data.List qualified as List
import Data.Map.Strict (lookup, traverseWithKey)
import Data.Map.Strict qualified as Map
import Data.Time qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import Database.PostgreSQL.Simple qualified as PG
import Protolude hiding ((%))

pickVariant :: VariantList -> EntryId -> FieldId -> VariantSelection
pickVariant variants entryId fieldId =
  let bs1 = UUID.toByteString $ getTyped @UUID entryId
      bs2 = UUID.toByteString $ getTyped @UUID fieldId
      nonModuloInt = B.decode @Int $ BS.L.fromStrict $ SHA256.hashlazy (bs1 <> bs2 <> salt)
      moduloInt = nonModuloInt `mod` length (getTyped @[Variant] variants)
   in case selectVariantByIdx variants moduloInt of
        Nothing -> panic "impossible"
        Just v -> v
  where
    salt = "Xpkl1YXi2i4k"

pickInt :: IntSchema -> EntryId -> FieldId -> BoundedInt
pickInt intSchema@IntSchema {intMin, intMax} entryId fieldId =
  let bs1 = UUID.toByteString $ getTyped @UUID entryId
      bs2 = UUID.toByteString $ getTyped @UUID fieldId
      nonModuloInt = B.decode @Int $ BS.L.fromStrict $ SHA256.hashlazy (bs1 <> bs2 <> salt)

      intSize = intMax - intMin
      intBase = nonModuloInt `mod` intSize
   in case mkBoundedInt intSchema (intMin + intBase) of
        Nothing -> panic "impossible"
        Just v -> v
  where
    salt = "Ofi5kaJxWS9N"

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

parseLedgerSchema :: LedgerId -> PublicLedgerSchema -> (FieldLabelIdMap, InternalLedgerSchema)
parseLedgerSchema lId labToF =
  let fIdsAndPairs = zip (idChainFromInit lId) (Map.toList labToF)
      labToId = fIdsAndPairs <&> \(fId, (lab, _sc)) -> (lab, fId)
      idToSc = fIdsAndPairs <&> \(fId, (_lab, sc)) -> (fId, sc)
   in (Map.fromList labToId, Map.fromList idToSc)

createLedger ::
  ( MonadIO m,
    MonadError e m,
    AsType (AggregateErrorWithState LedgerAggregateError) e,
    AsType WriteError e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  LedgerLabel ->
  PublicLedgerSchema ->
  m LedgerId
createLedger ledgerLabel schema = do
  evTime <- liftIO T.getCurrentTime
  ledgerId <- LedgerId <$> liftIO UUID.V4.nextRandom
  (agg, valAggId) <- fetchInitialLedgerAggById ledgerId
  let (labToId, idToFSc) = parseLedgerSchema ledgerId schema
  let newEvts = [LedgerCreated (LedgerCreatedEvent ledgerLabel idToFSc labToId)]
  void $ insertEventsValidated valAggId agg evTime newEvts
  pure ledgerId

summariseLedger ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState LedgerAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
  LedgerId ->
  m LedgerSummary
summariseLedger ledgerId = do
  (agg, _) <- fetchCreatedLedgerAggById ledgerId
  let labToId = getTyped @FieldLabelIdMap agg
  let entries =
        sortBy cmpEntryCreationTime (getField @"entries" agg)
          <&> \(id, (createTime, entryAgg)) -> toEntrySummary labToId id createTime entryAgg
  pure $
    LedgerSummary
      { id = ledgerId,
        nextEntryId = nextEntryIdFromAgg ledgerId agg,
        label = getTyped @LedgerLabel agg,
        schema = aggPublicSchema agg,
        entries
      }
  where
    cmpEntryCreationTime ::
      (EntryId, (T.UTCTime, EntryAggregate)) ->
      (EntryId, (T.UTCTime, EntryAggregate)) ->
      Ordering
    cmpEntryCreationTime (_, (tA, _)) (_, (tB, _)) = tA `compare` tB

toEntrySummary :: FieldLabelIdMap -> EntryId -> T.UTCTime -> EntryAggregate -> EntrySummary
toEntrySummary labToId id entryCreateTime entryAgg =
  EntrySummary
    { id,
      creationTime = entryCreateTime,
      body = makePublicEntry labToId (getField @"entry" entryAgg),
      isValid = case getTyped @EntryValidity entryAgg of
        EntryIsValid -> True
        EntryIsNotValid _ -> False,
      invalidationReason = case getTyped @EntryValidity entryAgg of
        EntryIsValid -> Nothing
        EntryIsNotValid reason -> Just reason
    }

recordEntry ::
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
recordEntry ledgerId reqEntryId reqBody = do
  (agg, valAggId) <- fetchCreatedLedgerAggById ledgerId
  nowTime <- liftIO T.getCurrentTime
  let ledgerSchema = getTyped @InternalLedgerSchema agg
  let labToId = getTyped @FieldLabelIdMap agg
  fLabToIdAndVal <- flip traverseWithKey reqBody $ \fieldLabel fieldValSpec -> do
    fieldId <- case lookup fieldLabel labToId of
      Nothing -> throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) (undefined :: LedgerAggregateError)
      Just v -> pure v

    fieldSchema <- case lookup fieldId ledgerSchema of
      Nothing -> throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) (undefined :: LedgerAggregateError)
      Just v -> pure v

    fieldVal <- case fieldValSpec of
      RandomFieldValSpec -> do
        let v = case fieldSchema of
              EnumFieldSchema varList ->
                EnumBody (pickVariant varList reqEntryId fieldId)
              IntFieldSchema intSchema -> do
                IntBody (pickInt intSchema reqEntryId fieldId)
        pure FieldVal {v, entryTime = nowTime}
      ManualFieldValSpec rawFieldVal -> do
        v <- case (fieldSchema, getTyped @RawFieldValBody rawFieldVal) of
          (EnumFieldSchema varList, RawEnumBody var) -> do
            case selectVariantByLabel varList var of
              Nothing ->
                throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) NoSuchVariant
              Just varSel ->
                pure (EnumBody varSel)
          (IntFieldSchema intSchema, RawIntBody intBody) -> do
            case mkBoundedInt intSchema intBody of
              Nothing ->
                throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) (undefined :: LedgerAggregateError)
              Just boundedInt ->
                pure (IntBody boundedInt)
          (_fSc, _fBody) ->
            throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) (undefined :: LedgerAggregateError)
        pure FieldVal {v, entryTime = getField @"entryTime" rawFieldVal}
    pure (fieldId, InternalFieldValBody <$> fieldVal)
  let internalEntry = Map.fromList (Map.elems fLabToIdAndVal)
  let publicEntry = fLabToIdAndVal <&> \(_fId, fVal) -> unInternalFieldValBody <$> fVal
  let newEvts = [EntryMade (EntryMadeEvent reqEntryId internalEntry)]
  void $ insertEventsValidated valAggId (Just agg) nowTime newEvts
  pure publicEntry

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
  let labToId = getTyped @FieldLabelIdMap agg
  case List.lookup reqEntryId (getField @"entries" agg) of
    Nothing ->
      throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) NoSuchEntry
    Just (decCreateTime, entryAgg) ->
      pure $ toEntrySummary labToId reqEntryId decCreateTime entryAgg

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
