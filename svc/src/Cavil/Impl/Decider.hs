{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Impl.Decider where

import Cavil.Api.Decider
import Cavil.Event.Common
import Cavil.Event.Decider
import Data.Generics.Product.Typed
import Data.Generics.Sum (AsType, injectTyped)
import qualified Data.List as List
import qualified Data.Time as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import qualified Database.PostgreSQL.Simple as PG
import Protolude hiding ((%))

pickVariant :: VariantList -> DecisionId -> Variant
pickVariant variants decisionId =
  let (w1, w2, w3, w4) = UUID.toWords $ getTyped @UUID decisionId
      nonModuloInt = abs $ sum $ fromIntegral @Word32 @Int <$> [w1, w2, w3, w4]
      varListList = getTyped @[Variant] variants
      moduloInt = nonModuloInt `mod` length varListList
   in case varListList `atMay` moduloInt of
        Nothing -> panic "impossible"
        Just v -> v

fetchInitialDeciderAggById ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState DeciderAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
  DeciderId ->
  m (Maybe DeciderAggregate, AggregateValidatedToken)
fetchInitialDeciderAggById deciderId =
  getAggregate (AggregateBeforeRequest (AggregateId (unDeciderId deciderId))) (Proxy @DeciderEvent)

fetchCreatedDeciderAggById ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState DeciderAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
  DeciderId ->
  m (DeciderAggregate, AggregateValidatedToken)
fetchCreatedDeciderAggById deciderId =
  fetchInitialDeciderAggById deciderId >>= \case
    (Nothing, valAggId) ->
      throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) NoSuchDecider
    (Just agg, valAggId) ->
      pure (agg, valAggId)

createDecider ::
  ( MonadIO m,
    MonadError e m,
    AsType (AggregateErrorWithState DeciderAggregateError) e,
    AsType WriteError e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  DeciderLabel ->
  VariantList ->
  m DeciderId
createDecider deciderLabel variants = do
  deciderId <- DeciderId <$> liftIO UUID.V4.nextRandom
  (agg, valAggId) <- fetchInitialDeciderAggById deciderId
  let newEvts = [DeciderCreated (DeciderCreatedEvent deciderLabel variants)]
  void $ insertEventsValidated valAggId agg newEvts
  pure deciderId

summariseDecider ::
  (MonadIO m, MonadError e m, AsType (AggregateErrorWithState DeciderAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
  DeciderId ->
  m DeciderSummary
summariseDecider deciderId = do
  (agg, _) <- fetchCreatedDeciderAggById deciderId

  let decisions =
        toDecisionSummary
          <$> sortBy cmpDecisionTime (getField @"decisions" agg)

  pure $
    DeciderSummary
      { id = deciderId,
        nextDecisionId = nextDecisionIdFromAgg deciderId agg,
        label = getTyped @DeciderLabel agg,
        variants = getTyped @VariantList agg,
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

recordDecisionRandom ::
  ( MonadIO m,
    MonadError e m,
    AsType (AggregateErrorWithState DeciderAggregateError) e,
    AsType WriteError e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  DeciderId ->
  DecisionId ->
  m Variant
recordDecisionRandom deciderId reqDecisionId = do
  (agg, valAggId) <- fetchCreatedDeciderAggById deciderId
  case List.lookup reqDecisionId (getField @"decisions" agg) of
    Nothing -> do
      nowTime <- liftIO T.getCurrentTime
      let decidedVariant = pickVariant (getTyped @VariantList agg) reqDecisionId

      let newEvts = [DecisionMade (DecisionMadeEvent reqDecisionId decidedVariant nowTime)]
      void $ insertEventsValidated valAggId (Just agg) newEvts

      pure decidedVariant
    Just existingDecAgg ->
      pure $ getTyped @Variant existingDecAgg

summariseDecision ::
  ( MonadIO m,
    MonadError e m,
    AsType (AggregateErrorWithState DeciderAggregateError) e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  DeciderId ->
  DecisionId ->
  m Variant
summariseDecision deciderId reqDecisionId = do
  (agg, valAggId) <- fetchCreatedDeciderAggById deciderId
  case List.lookup reqDecisionId (getField @"decisions" agg) of
    Nothing ->
      throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) NoSuchDecision
    Just existingDecAgg ->
      pure $ getTyped @Variant existingDecAgg

recordDecisionManual ::
  ( MonadIO m,
    MonadError e m,
    AsType (AggregateErrorWithState DeciderAggregateError) e,
    AsType WriteError e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  DeciderId ->
  DecisionId ->
  Variant ->
  T.UTCTime ->
  m Variant
recordDecisionManual deciderId reqDecisionId decidedVariant decisionTime = do
  (agg, valAggId) <- fetchCreatedDeciderAggById deciderId
  case List.lookup reqDecisionId (getField @"decisions" agg) of
    Nothing -> do
      let possVariants = getTyped @VariantList agg
      unless (decidedVariant `elem` (getTyped @[Variant] possVariants)) $
        throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) NoSuchVariant

      let newEvts = [DecisionMade (DecisionMadeEvent reqDecisionId decidedVariant decisionTime)]
      void $ insertEventsValidated valAggId (Just agg) newEvts
    Just existingDecAgg -> do
      unless (decidedVariant == getTyped @Variant existingDecAgg) $
        throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) InconsistentVariant
  pure decidedVariant

invalidateDecision ::
  ( MonadIO m,
    MonadError e m,
    AsType (AggregateErrorWithState DeciderAggregateError) e,
    AsType WriteError e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  DeciderId ->
  DecisionId ->
  Text ->
  m ()
invalidateDecision deciderId reqDecisionId invalidateReason = do
  (agg, valAggId) <- fetchCreatedDeciderAggById deciderId
  let newEvts = [DecisionInvalidated (DecisionInvalidatedEvent reqDecisionId invalidateReason)]
  void $ insertEventsValidated valAggId (Just agg) newEvts

getAllDeciderIds ::
  (MonadIO m, MonadReader r m, HasType PG.Connection r) =>
  m [DeciderId]
getAllDeciderIds =
  fmap (DeciderId . unAggregateId) <$> getAllAggIds (Proxy @DeciderEvent)
