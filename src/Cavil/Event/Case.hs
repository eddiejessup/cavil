{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Event.Case where

import Cavil.Api.Case
import Cavil.Event.Common
import Cavil.Hashing (nextDecisionToken)
import qualified Data.Aeson as Ae
import Data.Generics.Product.Typed
import Data.Generics.Sum (AsType, injectTyped)
import qualified Data.List as List
import Data.Time.Clock (UTCTime)
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import Optics
import Protolude hiding (to, (%))

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

initialCaseAggregate :: CaseCreatedEvent -> CaseAggregate
initialCaseAggregate ev =
  CaseAggregate
    { caseLabel = ev ^. typed @CaseLabel,
      nrVariants = ev ^. typed @NrVariants,
      decisions = []
    }

type DecisionsMap = [(DecisionToken, DecisionAggregate)]

-- | Do not filter to only valid decisions, or the token chain will lose
-- coherence.
lastDecisionToken :: CaseAggregate -> Maybe DecisionToken
lastDecisionToken agg =
  fst <$> lastMay (getTyped @DecisionsMap agg)

nextDecisionTokenFromAgg :: CaseAggregate -> DecisionToken
nextDecisionTokenFromAgg agg =
  Cavil.Hashing.nextDecisionToken $ case lastDecisionToken agg of
    Nothing -> Left $ getTyped @CaseLabel agg
    Just lastTok -> Right lastTok

foldCaseEvent :: (MonadError e m, AsType (AggregateErrorWithState CaseAggregateError) e) => AggregateState -> Maybe CaseAggregate -> CaseEvent -> m (Maybe CaseAggregate)
foldCaseEvent aggState mayAgg = \case
  CaseCreated ccEvt -> case mayAgg of
    Nothing ->
      pure $ Just $ initialCaseAggregate ccEvt
    Just _ ->
      throwError $ aggError CaseAlreadyExists
  DecisionMade ev -> do
    agg <- note (aggError NoSuchCase) mayAgg
    if nextDecisionTokenFromAgg agg == ev ^. typed @DecisionToken
      then pure $ Just $ agg & typed @DecisionsMap %~ (<> [(getTyped @DecisionToken ev, initialDecisionAggregate ev)])
      else throwError $ aggError IncoherentDecisionToken
  DecisionInvalidated ev -> do
    let tok = getTyped @DecisionToken ev
    let reason = getField @"reason" ev
    agg <- note (aggError NoSuchCase) mayAgg
    decisionAgg <- note (aggError NoSuchDecision) (List.lookup tok (getTyped @DecisionsMap agg))
    case getTyped @DecisionValidity decisionAgg of
      DecisionIsNotValid _ ->
        throwError $ aggError DecisionAlreadyInvalidated
      DecisionIsValid ->
        -- unsafeFiltered: "This is not a legal Traversal, unless you are very
        -- careful not to invalidate the predicate on the target." I am a
        -- careful boy: I only change the decision validity, while my
        -- predicate inspects only the decision token.
        pure $
          Just $
            agg
              & typed @DecisionsMap
              % traversed
              % unsafeFiltered (\(inTok, _) -> inTok == tok)
              % _2
              % typed @DecisionValidity
              .~ DecisionIsNotValid reason
  where
    aggError e =
      injectTyped $ AggregateErrorWithState aggState e

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
  { decisionToken :: DecisionToken,
    variant :: Variant,
    decisionTime :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON, Ae.FromJSON)

data DecisionInvalidatedEvent = DecisionInvalidatedEvent
  { decisionToken :: DecisionToken,
    reason :: Text
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON, Ae.FromJSON)

data CaseAggregate = CaseAggregate
  { caseLabel :: CaseLabel,
    nrVariants :: NrVariants,
    decisions :: DecisionsMap
  }
  deriving stock (Generic)

data CaseAggregateError
  = CaseAlreadyExists
  | NoSuchCase
  | NoSuchDecision
  | IncoherentDecisionToken
  | DecisionAlreadyInvalidated
  deriving stock (Generic, Show)

caseLabelsFromEvents :: [CaseEvent] -> [CaseLabel]
caseLabelsFromEvents = foldl' go []
  where
    go caseLabels = \case
      CaseCreated ccEvt -> caseLabels <> [getTyped @CaseLabel ccEvt]
      DecisionMade _ -> caseLabels
      DecisionInvalidated _ -> caseLabels

instance CavilEvent CaseEvent where
  type EvtAggregate CaseEvent = CaseAggregate

  type AggErr CaseEvent = CaseAggregateError

  eventType _pxy = "case"

  foldEvt = foldCaseEvent
