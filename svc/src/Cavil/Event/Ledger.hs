module Cavil.Event.Ledger where

import Cavil.Api.Ledger
import Cavil.Api.Ledger.Var
import Cavil.Event.Common
import Cavil.Hashing (nextEntryIdInChain)
import Data.Aeson qualified as Ae
import Data.Generics.Product.Typed
import Data.Generics.Sum (AsType, injectTyped)
import Data.List qualified as List
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromField qualified as PG
import Database.PostgreSQL.Simple.ToField qualified as PG
import Optics
import Protolude hiding (to, (%))

-- Event.

data LedgerEvent
  = LedgerCreated LedgerCreatedEvent
  | EntryMade EntryMadeEvent
  | EntryInvalidated EntryInvalidatedEvent
  deriving stock (Generic)

deriving anyclass instance Ae.ToJSON (LedgerEvent)

deriving anyclass instance Ae.FromJSON (LedgerEvent)

instance PG.FromField (LedgerEvent) where
  fromField = PG.fromJSONField

instance PG.ToField (LedgerEvent) where
  toField = PG.toJSONField

data LedgerCreatedEvent = LedgerCreatedEvent
  { ledgerLabel :: LedgerLabel,
    schema :: InternalLedgerSchema
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON, Ae.FromJSON)

type InternalLedgerSchema = Map FieldLabel (VariantList, FieldId)

type InternalEntry = Map FieldLabel (FieldVal VariantSelection)

data EntryMadeEvent = EntryMadeEvent
  { entryId :: EntryId,
    entry :: InternalEntry
  }
  deriving stock (Generic)

deriving anyclass instance Ae.ToJSON EntryMadeEvent

deriving anyclass instance Ae.FromJSON EntryMadeEvent

data EntryInvalidatedEvent = EntryInvalidatedEvent
  { entryId :: EntryId,
    reason :: Text
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON, Ae.FromJSON)

-- /Event.

-- Aggregate.

data LedgerAggregate = LedgerAggregate
  { ledgerLabel :: LedgerLabel,
    schema :: InternalLedgerSchema,
    entries :: EntriesMap
  }
  deriving stock (Generic)

initialLedgerAggregate :: LedgerCreatedEvent -> LedgerAggregate
initialLedgerAggregate ev =
  LedgerAggregate
    { ledgerLabel = getTyped @LedgerLabel ev,
      schema = getTyped @InternalLedgerSchema ev,
      entries = []
    }

type EntriesMap = [(EntryId, (UTCTime, EntryAggregate))]

data EntryAggregate = EntryAggregate
  { entry :: InternalEntry,
    status :: EntryValidity
  }
  deriving stock (Generic)

data EntryValidity
  = EntryIsValid
  | EntryIsNotValid Text

initialEntryAggregate :: EntryMadeEvent -> EntryAggregate
initialEntryAggregate ev =
  EntryAggregate
    { entry = getTyped @(InternalEntry) ev,
      status = EntryIsValid
    }

-- /Aggregate.

-- Folding events.

-- | Do not filter to only valid entries, or the ID chain will lose
-- coherence.
lastEntryId :: LedgerAggregate -> Maybe EntryId
lastEntryId agg =
  fst <$> lastMay (getTyped @EntriesMap agg)

nextEntryIdFromAgg :: LedgerId -> LedgerAggregate -> EntryId
nextEntryIdFromAgg ledgerId agg =
  Cavil.Hashing.nextEntryIdInChain $ case lastEntryId agg of
    Nothing -> Left ledgerId
    Just lastId -> Right lastId

foldLedgerEvent :: (MonadError e m, AsType (AggregateErrorWithState LedgerAggregateError) e) => AggregateState -> Maybe LedgerAggregate -> (LedgerEvent, UTCTime) -> m (Maybe LedgerAggregate)
foldLedgerEvent aggState mayAgg (evt, createdTime) = case evt of
  LedgerCreated ccEvt -> case mayAgg of
    Nothing ->
      pure $ Just $ initialLedgerAggregate ccEvt
    Just _ ->
      throwError $ aggError LedgerAlreadyExists
  EntryMade ev -> do
    agg <- note (aggError NoSuchLedger) mayAgg
    if nextEntryIdFromAgg ledgerId agg == ev ^. typed @EntryId
      then pure $ Just $ agg & typed @EntriesMap %~ (<> [(getTyped @EntryId ev, (createdTime, initialEntryAggregate ev))])
      else throwError $ aggError IncoherentEntryId
  EntryInvalidated ev -> do
    let dId = getTyped @EntryId ev
    let reason = getField @"reason" ev
    agg <- note (aggError NoSuchLedger) mayAgg
    (_entryCreatedTime, entryAgg) <- note (aggError NoSuchEntry) (List.lookup dId (getTyped @EntriesMap agg))
    case getTyped @EntryValidity entryAgg of
      EntryIsNotValid _ ->
        throwError $ aggError EntryAlreadyInvalidated
      EntryIsValid ->
        -- unsafeFiltered: "This is not a legal Traversal, unless you are very
        -- careful not to invalidate the predicate on the target." I am a
        -- careful boy: I only change the entry validity, while my
        -- predicate inspects only the entry ID.
        pure $
          Just $
            agg
              & typed @EntriesMap
              % traversed
              % unsafeFiltered (\(inId, _) -> inId == dId)
              % _2
              % _2
              % typed @EntryValidity
              .~ EntryIsNotValid reason
  where
    ledgerId = LedgerId $ unAggregateId $ aggIdFromState aggState

    aggError e =
      injectTyped $ AggregateErrorWithState aggState e

-- /Folding events.

-- Errors.

data LedgerAggregateError
  = LedgerAlreadyExists
  | NoSuchLedger
  | NoSuchEntry
  | NoSuchVariant
  | IncoherentEntryId
  | EntryAlreadyInvalidated
  | InconsistentVariant
  deriving stock (Generic, Show)

-- /Errors.

-- Projection.

ledgerLabelsFromEvents :: [LedgerEvent] -> [LedgerLabel]
ledgerLabelsFromEvents = foldl' go []
  where
    go ledgerLabels = \case
      LedgerCreated ccEvt -> ledgerLabels <> [getTyped @LedgerLabel ccEvt]
      EntryMade _ -> ledgerLabels
      EntryInvalidated _ -> ledgerLabels

-- /Projection.

-- Instance.

instance CavilEvent (LedgerEvent) where
  type EvtAggregate (LedgerEvent) = LedgerAggregate

  type AggErr (LedgerEvent) = LedgerAggregateError

  eventType _pxy = "ledger"

  foldEvt = foldLedgerEvent

-- /Instance.
