{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Impl.Case where

import Cavil.Api.Case
import Cavil.Event.Common
import Cavil.Event.Case
import Cavil.Hashing
import qualified Data.Binary as B
import Data.Generics.Product.Typed
import Data.Generics.Sum (AsType, injectTyped)
import qualified Data.List as List
import qualified Data.Time as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Database.PostgreSQL.Simple as PG
import Protolude hiding ((%))

aggIdFromCaseLabel :: CaseLabel -> AggregateID
aggIdFromCaseLabel caseLabel =
  AggregateID $
    uuidFromArbitraryByteString aggIdSalt $
      B.encode $
        getTyped @Text caseLabel
  where
    aggIdSalt = "7XUS608HTAPy"

pickVariant :: NrVariants -> DecisionToken -> Variant
pickVariant nrVariants decisionToken =
  let (w1, w2, w3, w4) = UUID.toWords $ getTyped @UUID decisionToken
      nonModuloInt = abs $ sum $ fromIntegral @Word32 @Int <$> [w1, w2, w3, w4]
      moduloInt = nonModuloInt `mod` getTyped @Int nrVariants
   in case mkVariant moduloInt of
        Nothing -> panic "impossible"
        Just v -> v

fetchInitialCaseAggByLabel ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState CaseAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
  CaseLabel ->
  m (Maybe CaseAggregate, AggregateValidatedToken)
fetchInitialCaseAggByLabel caseLabel =
  getAggregate (AggregateBeforeRequest (aggIdFromCaseLabel caseLabel)) (Proxy @CaseEvent)

fetchCreatedCaseAggByLabel ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState CaseAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
  CaseLabel ->
  m (CaseAggregate, AggregateValidatedToken)
fetchCreatedCaseAggByLabel caseLabel =
  fetchInitialCaseAggByLabel caseLabel >>= \case
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
  m ()
createCase caseLabel nrVariants = do
  (agg, valAggId) <- fetchInitialCaseAggByLabel caseLabel
  let newEvts = [CaseCreated (CaseCreatedEvent caseLabel nrVariants)]
  void $ insertEventsValidated valAggId agg newEvts

summariseCase ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState CaseAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
  CaseLabel ->
  m CaseSummary
summariseCase caseLabel = do
  (agg, _) <- fetchCreatedCaseAggByLabel caseLabel

  let decisions =
        toDecisionSummary
          <$> sortBy cmpDecisionTime (getField @"decisions" agg)

  pure $
    CaseSummary
      { nextDecisionToken = nextDecisionTokenFromAgg agg,
        label = getTyped @CaseLabel agg,
        nrVariants = getTyped @NrVariants agg,
        decisions
      }
  where
    cmpDecisionTime ::
      (DecisionToken, DecisionAggregate) ->
      (DecisionToken, DecisionAggregate) ->
      Ordering
    cmpDecisionTime (_, a) (_, b) = getField @"decisionTime" a `compare` getField @"decisionTime" b

    toDecisionSummary :: (DecisionToken, DecisionAggregate) -> DecisionSummary
    toDecisionSummary (token, decAgg) =
      DecisionSummary
        { token,
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
  CaseLabel ->
  DecisionToken ->
  m Variant
decideCase caseLabel reqDecisionToken = do
  (agg, valAggId) <- fetchCreatedCaseAggByLabel caseLabel
  case List.lookup reqDecisionToken (getField @"decisions" agg) of
    Nothing -> do
      nowTime <- liftIO T.getCurrentTime
      let decidedVariant = pickVariant (getTyped @NrVariants agg) reqDecisionToken

      let newEvts = [DecisionMade (DecisionMadeEvent reqDecisionToken decidedVariant nowTime)]
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
  CaseLabel ->
  DecisionToken ->
  Text ->
  m ()
invalidateDecision caseLabel reqDecisionToken invalidateReason = do
  (agg, valAggId) <- fetchCreatedCaseAggByLabel caseLabel
  let newEvts = [DecisionInvalidated (DecisionInvalidatedEvent reqDecisionToken invalidateReason)]
  void $ insertEventsValidated valAggId (Just agg) newEvts

getAllCaseLabels ::
  (MonadIO m, MonadReader r m, HasType PG.Connection r) =>
  m [CaseLabel]
getAllCaseLabels =
  caseLabelsFromEvents <$> getAllEvents
