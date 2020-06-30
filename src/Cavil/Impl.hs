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
import qualified Data.List as List
import qualified Data.Time as T
import qualified Database.PostgreSQL.Simple as PG
import Protolude hiding ((%))

data DecideDomainError
  deriving stock (Generic)

data CreateCaseDomainError
  deriving stock (Generic)

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
  pure $
    CaseSummary
      { nextDecisionToken = nextDecisionTokenFromAgg agg
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

  let existingDecisions =
        getTyped @[(DecisionToken, T.UTCTime, Variant)] agg <&> \(tok, _, var) -> (tok, var)
      mayExistingDecisionVariant = List.lookup reqDecisionToken existingDecisions

  case mayExistingDecisionVariant of
    Nothing -> do
      nowTime <- liftIO T.getCurrentTime
      let decidedVariant = pickVariant reqDecisionToken (getTyped @NrVariants agg)

      let newEvts = [DecisionMade (DecisionMadeEvent reqDecisionToken decidedVariant nowTime)]
      void $ insertEventsValidated valAggId (Just agg) newEvts

      pure decidedVariant
    Just existingDecision ->
      pure existingDecision

getAllCaseLabels ::
  (MonadIO m, MonadReader r m, HasType PG.Connection r) =>
  m [CaseLabel]
getAllCaseLabels =
  caseLabelsFromEvents <$> getAllEvents
