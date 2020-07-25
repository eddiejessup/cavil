{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Event.Ledger where

import Cavil.Api.Ledger
import Cavil.Event.Common
import qualified Data.Aeson as Ae
import Data.Generics.Product.Typed
import Data.Generics.Sum (AsType, injectTyped)
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import Optics
import Protolude hiding (to, (%))

-- Event.

data LedgerEvent
  = LedgerCreated LedgerCreatedEvent
  | RecordWritten RecordWrittenEvent
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON, Ae.FromJSON)

instance PG.FromField LedgerEvent where
  fromField = PG.fromJSONField

instance PG.ToField LedgerEvent where
  toField = PG.toJSONField

data LedgerCreatedEvent = LedgerCreatedEvent
  { label :: LedgerLabel
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON, Ae.FromJSON)

data RecordWrittenEvent = RecordWrittenEvent
  { recordId :: RecordId,
    recordTime :: RecordTime,
    body :: RecordBody
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON, Ae.FromJSON)

-- /Event.

-- Aggregate.

data LedgerAggregate = LedgerAggregate
  { label :: LedgerLabel,
    records :: RecordsMap
  }
  deriving stock (Generic)

initialLedgerAggregate :: LedgerCreatedEvent -> LedgerAggregate
initialLedgerAggregate ev =
  LedgerAggregate
    { label = ev ^. typed @LedgerLabel,
      records = []
    }

type RecordsMap = [(RecordId, RecordAggregate)]

data RecordAggregate = RecordAggregate
  { recordTime :: RecordTime,
    body :: RecordBody
  }
  deriving stock (Generic)

initialRecordAggregate :: RecordWrittenEvent -> RecordAggregate
initialRecordAggregate ev =
  RecordAggregate
    { recordTime = getTyped @RecordTime ev,
      body = getTyped @RecordBody ev
    }

-- /Aggregate.

-- Folding events.

-- | Do not filter to only valid decisions, or the ID chain will lose
-- coherence.
foldLedgerEvent :: (MonadError e m, AsType (AggregateErrorWithState LedgerAggregateError) e) => AggregateState -> Maybe LedgerAggregate -> LedgerEvent -> m (Maybe LedgerAggregate)
foldLedgerEvent aggState mayAgg = \case
  LedgerCreated createEvt -> case mayAgg of
    Nothing ->
      pure $ Just $ initialLedgerAggregate createEvt
    Just _ ->
      throwError $ aggError LedgerAlreadyExists
  RecordWritten ev -> do
    agg <- note (aggError NoSuchLedger) mayAgg
    pure $ Just $ agg & typed @RecordsMap %~ (<> [(getTyped @RecordId ev, initialRecordAggregate ev)])
  where
    aggError e =
      injectTyped $ AggregateErrorWithState aggState e

-- /Folding events.

-- Errors.

data LedgerAggregateError
  = LedgerAlreadyExists
  | NoSuchLedger
  deriving stock (Generic, Show)

-- /Errors.

-- Projection.

ledgerLabelsFromEvents :: [LedgerEvent] -> [LedgerLabel]
ledgerLabelsFromEvents = foldl' go []
  where
    go ledgerLabels = \case
      LedgerCreated createEvt -> ledgerLabels <> [getTyped @LedgerLabel createEvt]
      RecordWritten _ -> ledgerLabels

-- /Projection.

-- Instance.

instance CavilEvent LedgerEvent where
  type EvtAggregate LedgerEvent = LedgerAggregate

  type AggErr LedgerEvent = LedgerAggregateError

  eventType _pxy = "ledger"

  foldEvt = foldLedgerEvent

-- /Instance.
