{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Impl.Decider where

import Cavil.Api.Decider
import Cavil.Api.Decider.Var
import Cavil.Event.Common
import Cavil.Event.Decider
import Data.Generics.Product.Typed
import Data.Generics.Sum (AsType, injectTyped)
import qualified Data.List as List
import Data.Map.Strict (lookup, traverseWithKey)
import qualified Data.Time as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import qualified Database.PostgreSQL.Simple as PG
import Protolude hiding ((%))

pickVariant :: VariantList -> FieldId -> VariantSelection
pickVariant variants decisionId =
  let (w1, w2, w3, w4) = UUID.toWords $ getTyped @UUID decisionId
      nonModuloInt = abs $ sum $ fromIntegral @Word32 @Int <$> [w1, w2, w3, w4]
      moduloInt = nonModuloInt `mod` length (getTyped @[Variant] variants)
   in case selectVariantByIdx variants moduloInt of
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

parseDeciderSchema :: MonadIO m => DeciderSchema -> m InternalDeciderSchema
parseDeciderSchema = mapM withFieldId
  where
    withFieldId variants = do
      fieldId <- FieldId <$> liftIO UUID.V4.nextRandom
      pure (variants, fieldId)

createDecider ::
  ( MonadIO m,
    MonadError e m,
    AsType (AggregateErrorWithState DeciderAggregateError) e,
    AsType WriteError e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  DeciderLabel ->
  DeciderSchema ->
  m DeciderId
createDecider deciderLabel schema = do
  deciderId <- DeciderId <$> liftIO UUID.V4.nextRandom
  (agg, valAggId) <- fetchInitialDeciderAggById deciderId
  internalSchema <- parseDeciderSchema schema
  let newEvts = [DeciderCreated (DeciderCreatedEvent deciderLabel internalSchema)]
  void $ insertEventsValidated valAggId agg newEvts
  pure deciderId

-- summariseDecider ::
--   (MonadIO m, MonadError e m, AsType (AggregateErrorWithState DeciderAggregateError) e, MonadReader r m, HasType PG.Connection r) =>
--   DeciderId ->
--   m DeciderSummary
-- summariseDecider deciderId = do
--   (agg, _) <- fetchCreatedDeciderAggById deciderId

--   let decisions =
--         toDecisionSummary
--           <$> sortBy cmpDecisionTime (getField @"decisions" agg)

--   pure $
--     DeciderSummary
--       { id = deciderId,
--         nextDecisionId = nextDecisionIdFromAgg deciderId agg,
--         label = getTyped @DeciderLabel agg,
--         schema = getTyped @DeciderSchema agg,
--         decisions
--       }
--   where
--     cmpDecisionTime ::
--       (DecisionId, DecisionAggregate) ->
--       (DecisionId, DecisionAggregate) ->
--       Ordering
--     cmpDecisionTime (_, a) (_, b) = getField @"decisionTime" a `compare` getField @"decisionTime" b

--     toDecisionSummary :: (DecisionId, DecisionAggregate) -> DecisionSummary
--     toDecisionSummary (id, decAgg) =
--       DecisionSummary
--         { id,
--           decisionTime = getField @"decisionTime" decAgg,
--           variant = getTyped @Variant decAgg,
--           isValid = case getTyped @DecisionValidity decAgg of
--             DecisionIsValid -> True
--             DecisionIsNotValid _ -> False,
--           invalidationReason = case getTyped @DecisionValidity decAgg of
--             DecisionIsValid -> Nothing
--             DecisionIsNotValid reason -> Just reason
--         }

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
  RecordDecisionRequest ->
  m RecordDecisionResponse
recordDecisionRandom deciderId reqDecisionId reqBody = do
  (agg, valAggId) <- fetchCreatedDeciderAggById deciderId
  case List.lookup reqDecisionId (getField @"decisions" agg) of
    Nothing -> do
      nowTime <- liftIO T.getCurrentTime
      let schema = getTyped @InternalDeciderSchema agg
      decision <- flip traverseWithKey reqBody $ \fieldLabel decVar -> do
        (variants, fieldId) <- case lookup fieldLabel schema of
          Nothing -> throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) (undefined :: DeciderAggregateError)
          Just v -> pure v
        case decVar of
          RandomDecisionVar ->
            pure EvalVar {v = pickVariant variants fieldId, decisionTime = nowTime}
          ManualDecisionVar ev ->
            case collectMaybeEvalVar (selectVariantByLabel variants) ev of
              Nothing ->
                throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) NoSuchVariant
              Just a ->
                pure a
      let newEvts = [DecisionMade (DecisionMadeEvent reqDecisionId decision)]
      void $ insertEventsValidated valAggId (Just agg) newEvts
      pure decision
    Just existingDecAgg ->
      pure $ getTyped @InternalDecision existingDecAgg

summariseDecision ::
  ( MonadIO m,
    MonadError e m,
    AsType (AggregateErrorWithState DeciderAggregateError) e,
    MonadReader r m,
    HasType PG.Connection r
  ) =>
  DeciderId ->
  DecisionId ->
  m InternalDecision
summariseDecision deciderId reqDecisionId = do
  (agg, valAggId) <- fetchCreatedDeciderAggById deciderId
  case List.lookup reqDecisionId (getField @"decisions" agg) of
    Nothing ->
      throwError $ injectTyped $ AggregateErrorWithState (AggregateDuringRequest valAggId) NoSuchDecision
    Just existingDecAgg ->
      pure $ getTyped @InternalDecision existingDecAgg

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
