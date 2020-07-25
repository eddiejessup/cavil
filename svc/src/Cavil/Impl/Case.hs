{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Impl.Case where

import Cavil.Api.Case
import Cavil.Event.Case
import Cavil.Event.Common
import Data.Generics.Product.Typed
import Data.Generics.Sum (AsType, injectTyped)
import qualified Data.List as List
import qualified Data.Time as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import qualified Database.PostgreSQL.Simple as PG
import Protolude hiding ((%))

pickVariant :: NrVariants -> DecisionId -> Variant
pickVariant nrVariants decisionId =
  let (w1, w2, w3, w4) = UUID.toWords $ getTyped @UUID decisionId
      nonModuloInt = abs $ sum $ fromIntegral @Word32 @Int <$> [w1, w2, w3, w4]
      moduloInt = nonModuloInt `mod` getTyped @Int nrVariants
   in case mkVariant moduloInt of
        Nothing -> panic "impossible"
        Just v -> v

fetchInitialCaseAggById ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState CaseAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
  CaseId ->
  m (Maybe CaseAggregate, AggregateValidatedToken)
fetchInitialCaseAggById caseId =
  getAggregate (AggregateBeforeRequest (AggregateId (unCaseId caseId))) (Proxy @CaseEvent)

fetchCreatedCaseAggById ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState CaseAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
  CaseId ->
  m (CaseAggregate, AggregateValidatedToken)
fetchCreatedCaseAggById caseId =
  fetchInitialCaseAggById caseId >>= \case
    (Nothing, valAggId) ->
      throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) NoSuchCase
    (Just agg, valAggId) ->
      pure (agg, valAggId)

createCase ::
  ( MonadIO m,
    MonadError e m,
    AsType (AggregateErrorWithState CaseAggregateError) e,
    AsType WriteError e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  CaseLabel ->
  NrVariants ->
  m CaseId
createCase caseLabel nrVariants = do
  caseId <- CaseId <$> liftIO UUID.V4.nextRandom
  (agg, valAggId) <- fetchInitialCaseAggById caseId
  let newEvts = [CaseCreated (CaseCreatedEvent caseLabel nrVariants)]
  void $ insertEventsValidated valAggId agg newEvts
  pure caseId

summariseCase ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState CaseAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
  CaseId ->
  m CaseSummary
summariseCase caseId = do
  (agg, _) <- fetchCreatedCaseAggById caseId

  let decisions =
        toDecisionSummary
          <$> sortBy cmpDecisionTime (getField @"decisions" agg)

  pure $
    CaseSummary
      { id = caseId,
        nextDecisionId = nextDecisionIdFromAgg caseId agg,
        label = getTyped @CaseLabel agg,
        nrVariants = getTyped @NrVariants agg,
        decisions
      }
  where
    cmpDecisionTime ::
      (DecisionId, DecisionAggregate) ->
      (DecisionId, DecisionAggregate) ->
      Ordering
    cmpDecisionTime (_, a) (_, b) = getField @"decisionTime" a `compare` getField @"decisionTime" b

    toDecisionSummary :: (DecisionId, DecisionAggregate) -> DecisionSummary
    toDecisionSummary (id, decAgg) =
      DecisionSummary
        { id,
          decisionTime = getField @"decisionTime" decAgg,
          variant = getTyped @Variant decAgg,
          isValid = case getTyped @DecisionValidity decAgg of
            DecisionIsValid -> True
            DecisionIsNotValid _ -> False,
          invalidationReason = case getTyped @DecisionValidity decAgg of
            DecisionIsValid -> Nothing
            DecisionIsNotValid reason -> Just reason
        }

decideCase ::
  ( MonadIO m,
    MonadError e m,
    AsType (AggregateErrorWithState CaseAggregateError) e,
    AsType WriteError e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  CaseId ->
  DecisionId ->
  m Variant
decideCase caseId reqDecisionId = do
  (agg, valAggId) <- fetchCreatedCaseAggById caseId
  case List.lookup reqDecisionId (getField @"decisions" agg) of
    Nothing -> do
      nowTime <- liftIO T.getCurrentTime
      let decidedVariant = pickVariant (getTyped @NrVariants agg) reqDecisionId

      let newEvts = [DecisionMade (DecisionMadeEvent reqDecisionId decidedVariant nowTime)]
      void $ insertEventsValidated valAggId (Just agg) newEvts

      pure decidedVariant
    Just existingDecAgg ->
      pure $ getTyped @Variant existingDecAgg

invalidateDecision ::
  ( MonadIO m,
    MonadError e m,
    AsType (AggregateErrorWithState CaseAggregateError) e,
    AsType WriteError e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  CaseId ->
  DecisionId ->
  Text ->
  m ()
invalidateDecision caseId reqDecisionId invalidateReason = do
  (agg, valAggId) <- fetchCreatedCaseAggById caseId
  let newEvts = [DecisionInvalidated (DecisionInvalidatedEvent reqDecisionId invalidateReason)]
  void $ insertEventsValidated valAggId (Just agg) newEvts

getAllCaseIds ::
  (MonadIO m, MonadReader r m, HasType PG.Connection r) =>
  m [CaseId]
getAllCaseIds =
  fmap (CaseId . unAggregateId) <$> getAllAggIds (Proxy @CaseEvent)
