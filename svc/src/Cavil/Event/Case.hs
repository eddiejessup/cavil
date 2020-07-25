{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Event.Case where

import Cavil.Api.Case
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

data CaseEvent
  = CaseCreated CaseCreatedEvent
  | DecisionMade DecisionMadeEvent
  | DecisionInvalidated DecisionInvalidatedEvent
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON, Ae.FromJSON)

instance PG.FromField CaseEvent where
  fromField = PG.fromJSONField

instance PG.ToField CaseEvent where
  toField = PG.toJSONField

data CaseCreatedEvent = CaseCreatedEvent
  { caseLabel :: CaseLabel,
    nrVariants :: NrVariants
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

data CaseAggregate = CaseAggregate
  { caseLabel :: CaseLabel,
    nrVariants :: NrVariants,
    decisions :: DecisionsMap
  }
  deriving stock (Generic)

initialCaseAggregate :: CaseCreatedEvent -> CaseAggregate
initialCaseAggregate ev =
  CaseAggregate
    { caseLabel = ev ^. typed @CaseLabel,
      nrVariants = ev ^. typed @NrVariants,
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
lastDecisionId :: CaseAggregate -> Maybe DecisionId
lastDecisionId agg =
  fst <$> lastMay (getTyped @DecisionsMap agg)

nextDecisionIdFromAgg :: CaseId -> CaseAggregate -> DecisionId
nextDecisionIdFromAgg caseId agg =
  Cavil.Hashing.nextDecisionId $ case lastDecisionId agg of
    Nothing -> Left caseId
    Just lastId -> Right lastId

foldCaseEvent :: (MonadError e m, AsType (AggregateErrorWithState CaseAggregateError) e) => AggregateState -> Maybe CaseAggregate -> CaseEvent -> m (Maybe CaseAggregate)
foldCaseEvent aggState mayAgg = \case
  CaseCreated ccEvt -> case mayAgg of
    Nothing ->
      pure $ Just $ initialCaseAggregate ccEvt
    Just _ ->
      throwError $ aggError CaseAlreadyExists
  DecisionMade ev -> do
    agg <- note (aggError NoSuchCase) mayAgg
    if nextDecisionIdFromAgg caseId agg == ev ^. typed @DecisionId
      then pure $ Just $ agg & typed @DecisionsMap %~ (<> [(getTyped @DecisionId ev, initialDecisionAggregate ev)])
      else throwError $ aggError IncoherentDecisionId
  DecisionInvalidated ev -> do
    let dId = getTyped @DecisionId ev
    let reason = getField @"reason" ev
    agg <- note (aggError NoSuchCase) mayAgg
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
    caseId = CaseId $ unAggregateId $ aggIdFromState aggState

    aggError e =
      injectTyped $ AggregateErrorWithState aggState e

-- /Folding events.

-- Errors.

data CaseAggregateError
  = CaseAlreadyExists
  | NoSuchCase
  | NoSuchDecision
  | IncoherentDecisionId
  | DecisionAlreadyInvalidated
  deriving stock (Generic, Show)

-- /Errors.

-- Projection.

caseLabelsFromEvents :: [CaseEvent] -> [CaseLabel]
caseLabelsFromEvents = foldl' go []
  where
    go caseLabels = \case
      CaseCreated ccEvt -> caseLabels <> [getTyped @CaseLabel ccEvt]
      DecisionMade _ -> caseLabels
      DecisionInvalidated _ -> caseLabels

-- /Projection.

-- Instance.

instance CavilEvent CaseEvent where
  type EvtAggregate CaseEvent = CaseAggregate

  type AggErr CaseEvent = CaseAggregateError

  eventType _pxy = "case"

  foldEvt = foldCaseEvent

-- /Instance.
