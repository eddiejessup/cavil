{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Impl where

import Cavil.Api
import Cavil.Event
import Cavil.Hashing
import Cavil.Project
import Data.Generics.Product.Typed
import Data.Generics.Sum (AsType, injectTyped)
import qualified Data.Time as T
import qualified Data.Time.Format.ISO8601 as T
import qualified Database.PostgreSQL.Simple as PG
import Protolude hiding ((%))
import qualified Data.Map.Strict as Map

aggIdFromCaseLabel :: CaseLabel -> AggregateID
aggIdFromCaseLabel caseLabel =
  AggregateID $ uuidFromArbitraryText $ getTyped @Text caseLabel

fetchInitialAggByLabel ::
  (MonadIO m, MonadError e m, AsType AggregateError e, MonadReader r m, HasType PG.Connection r) =>
  CaseLabel ->
  m (Maybe Aggregate, AggregateValidatedToken)
fetchInitialAggByLabel caseLabel =
  getAggregate (AggregateBeforeRequest (aggIdFromCaseLabel caseLabel))

fetchCreatedAggByLabel ::
  (MonadIO m, MonadError e m, AsType AggregateError e, MonadReader r m, HasType PG.Connection r) =>
  CaseLabel ->
  m (Aggregate, AggregateValidatedToken)
fetchCreatedAggByLabel caseLabel =
  fetchInitialAggByLabel caseLabel >>= \case
    (Nothing, valAggId) ->
      throwError $ injectTyped $ AggregateError (AggregateDuringRequest valAggId) NoSuchCase
    (Just agg, valAggId) ->
      pure (agg, valAggId)

createCase ::
  ( MonadIO m,
    MonadError e m,
    AsType AggregateError e,
    AsType WriteError e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  CaseLabel ->
  NrVariants ->
  m ()
createCase caseLabel nrVariants = do
  (agg, valAggId) <- fetchInitialAggByLabel caseLabel
  let newEvts = [CaseCreated (CaseCreatedEvent caseLabel nrVariants)]
  void $ insertEventsValidated valAggId agg newEvts

summariseCase ::
  (MonadIO m, MonadError e m, AsType AggregateError e, MonadReader r m, HasType PG.Connection r) =>
  CaseLabel ->
  m CaseSummary
summariseCase caseLabel = do
  (agg, _) <- fetchCreatedAggByLabel caseLabel

  let
    decisions = toDecisionSummary
      <$> sortBy cmpDecisionTime (Map.assocs (getField @"decisions" agg))

  pure $
    CaseSummary
      { nextDecisionToken = nextDecisionTokenFromAgg agg,
        label = getTyped @CaseLabel agg,
        nrVariants = getTyped @NrVariants agg,
        decisions
      }
  where
    cmpDecisionTime ::
      (DecisionToken, (T.UTCTime, Variant, IsDecisionValid)) ->
      (DecisionToken, (T.UTCTime, Variant, IsDecisionValid)) ->
      Ordering
    cmpDecisionTime (_, (a, _, _)) (_, (b, _, _)) = a `compare` b

toDecisionSummary :: (DecisionToken, (T.UTCTime, Variant, IsDecisionValid)) -> DecisionSummary
toDecisionSummary (token, (decisionTimeUTC, variant, isDecisionValid)) =
  DecisionSummary
    { token,
      decisionTimeUTC = toS $ T.iso8601Show decisionTimeUTC,
      variant,
      isValid = case isDecisionValid of
        DecisionIsValid -> True
        DecisionIsNotValid -> False
    }

decideCase ::
  ( MonadIO m,
    MonadError e m,
    AsType AggregateError e,
    AsType WriteError e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  CaseLabel ->
  DecisionToken ->
  m Variant
decideCase caseLabel reqDecisionToken = do
  (agg, valAggId) <- fetchCreatedAggByLabel caseLabel
  case Map.lookup reqDecisionToken (getField @"decisions" agg) of
    Nothing -> do
      nowTime <- liftIO T.getCurrentTime
      let decidedVariant = pickVariant reqDecisionToken (getTyped @NrVariants agg)

      let newEvts = [DecisionMade (DecisionMadeEvent reqDecisionToken decidedVariant nowTime)]
      void $ insertEventsValidated valAggId (Just agg) newEvts

      pure decidedVariant
    Just (_, existingDecision, _) ->
      pure existingDecision

invalidateDecision ::
  ( MonadIO m,
    MonadError e m,
    AsType AggregateError e,
    AsType WriteError e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  CaseLabel ->
  DecisionToken ->
  Text ->
  m ()
invalidateDecision caseLabel reqDecisionToken invalidateReason = do
  (agg, valAggId) <- fetchCreatedAggByLabel caseLabel
  let newEvts = [DecisionInvalidated (DecisionInvalidatedEvent reqDecisionToken invalidateReason)]
  void $ insertEventsValidated valAggId (Just agg) newEvts

getAllCaseLabels ::
  (MonadIO m, MonadReader r m, HasType PG.Connection r) =>
  m [CaseLabel]
getAllCaseLabels =
  caseLabelsFromEvents <$> getAllEvents
