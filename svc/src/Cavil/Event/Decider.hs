module Cavil.Event.Decider where

import Cavil.Api.Decider
import Cavil.Api.Decider.Var
import Cavil.Event.Common
import Cavil.Hashing (nextDecisionId)
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

data DeciderEvent
  = DeciderCreated DeciderCreatedEvent
  | DecisionMade DecisionMadeEvent
  | DecisionInvalidated DecisionInvalidatedEvent
  deriving stock (Generic)

deriving anyclass instance Ae.ToJSON (DeciderEvent)

deriving anyclass instance Ae.FromJSON (DeciderEvent)

instance PG.FromField (DeciderEvent) where
  fromField = PG.fromJSONField

instance PG.ToField (DeciderEvent) where
  toField = PG.toJSONField

data DeciderCreatedEvent = DeciderCreatedEvent
  { deciderLabel :: DeciderLabel,
    schema :: InternalDeciderSchema
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON, Ae.FromJSON)

type InternalDeciderSchema = Map FieldLabel (VariantList, FieldId)

type InternalDecision = Map FieldLabel (EvalVar VariantSelection)

data DecisionMadeEvent = DecisionMadeEvent
  { decisionId :: DecisionId,
    decision :: InternalDecision
  }
  deriving stock (Generic)

deriving anyclass instance Ae.ToJSON DecisionMadeEvent

deriving anyclass instance Ae.FromJSON DecisionMadeEvent

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
    schema :: InternalDeciderSchema,
    decisions :: DecisionsMap
  }
  deriving stock (Generic)

initialDeciderAggregate :: DeciderCreatedEvent -> DeciderAggregate
initialDeciderAggregate ev =
  DeciderAggregate
    { deciderLabel = getTyped @DeciderLabel ev,
      schema = getTyped @InternalDeciderSchema ev,
      decisions = []
    }

type DecisionsMap = [(DecisionId, (UTCTime, DecisionAggregate))]

data DecisionAggregate = DecisionAggregate
  { decision :: InternalDecision,
    status :: DecisionValidity
  }
  deriving stock (Generic)

data DecisionValidity
  = DecisionIsValid
  | DecisionIsNotValid Text

initialDecisionAggregate :: DecisionMadeEvent -> DecisionAggregate
initialDecisionAggregate ev =
  DecisionAggregate
    { decision = getTyped @(InternalDecision) ev,
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

foldDeciderEvent :: (MonadError e m, AsType (AggregateErrorWithState DeciderAggregateError) e) => AggregateState -> Maybe DeciderAggregate -> (DeciderEvent, UTCTime) -> m (Maybe DeciderAggregate)
foldDeciderEvent aggState mayAgg (evt, createdTime) = case evt of
  DeciderCreated ccEvt -> case mayAgg of
    Nothing ->
      pure $ Just $ initialDeciderAggregate ccEvt
    Just _ ->
      throwError $ aggError DeciderAlreadyExists
  DecisionMade ev -> do
    agg <- note (aggError NoSuchDecider) mayAgg
    if nextDecisionIdFromAgg deciderId agg == ev ^. typed @DecisionId
      then pure $ Just $ agg & typed @DecisionsMap %~ (<> [(getTyped @DecisionId ev, (createdTime, initialDecisionAggregate ev))])
      else throwError $ aggError IncoherentDecisionId
  DecisionInvalidated ev -> do
    let dId = getTyped @DecisionId ev
    let reason = getField @"reason" ev
    agg <- note (aggError NoSuchDecider) mayAgg
    (_decisionCreatedTime, decisionAgg) <- note (aggError NoSuchDecision) (List.lookup dId (getTyped @DecisionsMap agg))
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

instance CavilEvent (DeciderEvent) where
  type EvtAggregate (DeciderEvent) = DeciderAggregate

  type AggErr (DeciderEvent) = DeciderAggregateError

  eventType _pxy = "decider"

  foldEvt = foldDeciderEvent

-- /Instance.
