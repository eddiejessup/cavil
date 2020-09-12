{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Event.Decider where

import Cavil.Api.Decider
import Cavil.Event.Common
import Cavil.Hashing (nextDecisionId)
import qualified Data.Aeson as Ae
import Data.Generics.Product.Typed
import Data.Generics.Sum (AsType, injectTyped)
import qualified Data.List as List
import Data.Time.Clock (UTCTime)
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import Optics
import Protolude hiding (to, (%))

-- Event.

data DeciderEvent
  = DeciderCreated DeciderCreatedEvent
  | DecisionMade DecisionMadeEvent
  | DecisionInvalidated DecisionInvalidatedEvent
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON, Ae.FromJSON)

instance PG.FromField DeciderEvent where
  fromField = PG.fromJSONField

instance PG.ToField DeciderEvent where
  toField = PG.toJSONField

data DeciderCreatedEvent = DeciderCreatedEvent
  { deciderLabel :: DeciderLabel,
    variants :: VariantList
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON, Ae.FromJSON)

data DecisionMadeEvent = DecisionMadeEvent
  { decisionId :: DecisionId,
    variant :: Variant,
    decisionTime :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON, Ae.FromJSON)

data DecisionInvalidatedEvent = DecisionInvalidatedEvent
  { decisionId :: DecisionId,
    reason :: Text
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON, Ae.FromJSON)

-- /Event.

-- Aggregate.

data DeciderAggregate = DeciderAggregate
  { deciderLabel :: DeciderLabel,
    variants :: VariantList,
    decisions :: DecisionsMap
  }
  deriving stock (Generic)

initialDeciderAggregate :: DeciderCreatedEvent -> DeciderAggregate
initialDeciderAggregate ev =
  DeciderAggregate
    { deciderLabel = ev ^. typed @DeciderLabel,
      variants = ev ^. typed @VariantList,
      decisions = []
    }

type DecisionsMap = [(DecisionId, DecisionAggregate)]

data DecisionAggregate = DecisionAggregate
  { decisionTime :: UTCTime,
    variant :: Variant,
    status :: DecisionValidity
  }
  deriving stock (Generic)

data DecisionValidity
  = DecisionIsValid
  | DecisionIsNotValid Text

initialDecisionAggregate :: DecisionMadeEvent -> DecisionAggregate
initialDecisionAggregate ev =
  DecisionAggregate
    { decisionTime = getTyped @UTCTime ev,
      variant = getTyped @Variant ev,
      status = DecisionIsValid
    }

-- /Aggregate.

-- Folding events.

-- | Do not filter to only valid decisions, or the ID chain will lose
-- coherence.
lastDecisionId :: DeciderAggregate -> Maybe DecisionId
lastDecisionId agg =
  fst <$> lastMay (getTyped @DecisionsMap agg)

nextDecisionIdFromAgg :: DeciderId -> DeciderAggregate -> DecisionId
nextDecisionIdFromAgg deciderId agg =
  Cavil.Hashing.nextDecisionId $ case lastDecisionId agg of
    Nothing -> Left deciderId
    Just lastId -> Right lastId

foldDeciderEvent :: (MonadError e m, AsType (AggregateErrorWithState DeciderAggregateError) e) => AggregateState -> Maybe DeciderAggregate -> DeciderEvent -> m (Maybe DeciderAggregate)
foldDeciderEvent aggState mayAgg = \case
  DeciderCreated ccEvt -> case mayAgg of
    Nothing ->
      pure $ Just $ initialDeciderAggregate ccEvt
    Just _ ->
      throwError $ aggError DeciderAlreadyExists
  DecisionMade ev -> do
    agg <- note (aggError NoSuchDecider) mayAgg
    if nextDecisionIdFromAgg deciderId agg == ev ^. typed @DecisionId
      then pure $ Just $ agg & typed @DecisionsMap %~ (<> [(getTyped @DecisionId ev, initialDecisionAggregate ev)])
      else throwError $ aggError IncoherentDecisionId
  DecisionInvalidated ev -> do
    let dId = getTyped @DecisionId ev
    let reason = getField @"reason" ev
    agg <- note (aggError NoSuchDecider) mayAgg
    decisionAgg <- note (aggError NoSuchDecision) (List.lookup dId (getTyped @DecisionsMap agg))
    case getTyped @DecisionValidity decisionAgg of
      DecisionIsNotValid _ ->
        throwError $ aggError DecisionAlreadyInvalidated
      DecisionIsValid ->
        -- unsafeFiltered: "This is not a legal Traversal, unless you are very
        -- careful not to invalidate the predicate on the target." I am a
        -- careful boy: I only change the decision validity, while my
        -- predicate inspects only the decision ID.
        pure $
          Just $
            agg
              & typed @DecisionsMap
              % traversed
              % unsafeFiltered (\(inId, _) -> inId == dId)
              % _2
              % typed @DecisionValidity
              .~ DecisionIsNotValid reason
  where
    deciderId = DeciderId $ unAggregateId $ aggIdFromState aggState

    aggError e =
      injectTyped $ AggregateErrorWithState aggState e

-- /Folding events.

-- Errors.

data DeciderAggregateError
  = DeciderAlreadyExists
  | NoSuchDecider
  | NoSuchDecision
  | NoSuchVariant
  | IncoherentDecisionId
  | DecisionAlreadyInvalidated
  | InconsistentVariant
  deriving stock (Generic, Show)

-- /Errors.

-- Projection.

deciderLabelsFromEvents :: [DeciderEvent] -> [DeciderLabel]
deciderLabelsFromEvents = foldl' go []
  where
    go deciderLabels = \case
      DeciderCreated ccEvt -> deciderLabels <> [getTyped @DeciderLabel ccEvt]
      DecisionMade _ -> deciderLabels
      DecisionInvalidated _ -> deciderLabels

-- /Projection.

-- Instance.

instance CavilEvent DeciderEvent where
  type EvtAggregate DeciderEvent = DeciderAggregate

  type AggErr DeciderEvent = DeciderAggregateError

  eventType _pxy = "decider"

  foldEvt = foldDeciderEvent

-- /Instance.
