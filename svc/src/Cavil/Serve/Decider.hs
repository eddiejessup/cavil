{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Serve.Decider where

import Cavil.Api
import Cavil.Api.Common
import Cavil.Api.Decider
import Cavil.Event.Common
import Cavil.Event.Decider
import Cavil.Impl.Decider
import Cavil.Serve.Common
import Data.Aeson ((.=))
import Data.Generics.Product.Typed
import Protolude
import Servant
import Servant.Server.Generic

data CreateDeciderError
  = CreateDeciderAggregateError (AggregateErrorWithState DeciderAggregateError)
  | CreateDeciderWriteError WriteError
  deriving stock (Generic)

data DeciderSummaryError
  = DeciderSummaryAggregateError (AggregateErrorWithState DeciderAggregateError)
  deriving stock (Generic)

data DecideError
  = DecideAggregateError (AggregateErrorWithState DeciderAggregateError)
  | DecideWriteError WriteError
  deriving stock (Generic)

data DecisionSummaryError
  = DecisionSummaryAggregateError (AggregateErrorWithState DeciderAggregateError)
  deriving stock (Generic)

data InvalidateDecisionError
  = InvalidateDecisionAggregateError (AggregateErrorWithState DeciderAggregateError)
  | InvalidateDecisionWriteError WriteError
  deriving stock (Generic)

mapAggregateError :: Maybe DeciderId -> AggregateErrorWithState DeciderAggregateError -> ClientError
mapAggregateError mayDeciderId (AggregateErrorWithState aggState detail) =
  case aggState of
    AggregateBeforeRequest _ ->
      let detailMsg = case detail of
            DeciderAlreadyExists ->
              "Multiple decider creation events"
            NoSuchDecider ->
              "Decider events before decider creation"
            NoSuchDecision ->
              "Decision events before decision creation"
            NoSuchVariant ->
              "Unknown variants in decision chain"
            IncoherentDecisionId ->
              "Incoherent IDs in decision chain"
            DecisionAlreadyInvalidated ->
              "Multiple invalidations of decision"
            InconsistentVariant ->
              "Ambiguous decision-variant"
       in simpleClientError OurFault detailMsg vals
    AggregateDuringRequest _ ->
      let detailMsg = case detail of
            DeciderAlreadyExists ->
              "Decider already exists"
            NoSuchDecider ->
              "No such decider found"
            NoSuchDecision ->
              "No such decision found"
            NoSuchVariant ->
              "Variant not an option"
            IncoherentDecisionId ->
              "Incoherent decision ID"
            DecisionAlreadyInvalidated ->
              "Decision has already been invalidated"
            InconsistentVariant ->
              "Manual decision-variant is inconsistent with existing variant"
       in simpleClientError OurFault detailMsg vals
  where
    vals = maybe mempty ("deciderId" .=) mayDeciderId

deciderCreate ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  CreateDeciderRequest ->
  m DeciderId
deciderCreate _user ccReq =
  runExceptT (createDecider (getTyped @DeciderLabel ccReq) (getTyped @NrVariants ccReq)) >>= \case
    Left e -> throwError $
      clientErrorAsServantError $ case e of
        CreateDeciderAggregateError aggE -> mapAggregateError Nothing aggE
        CreateDeciderWriteError we -> mapWriteError we
    Right v ->
      pure v

deciderSummarise ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  DeciderId ->
  m DeciderSummary
deciderSummarise _user deciderId =
  runExceptT (summariseDecider deciderId) >>= \case
    Left e -> throwError $
      clientErrorAsServantError $ case e of
        DeciderSummaryAggregateError aggE -> mapAggregateError (Just deciderId) aggE
    Right v ->
      pure v

deciderDecide ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  DeciderId ->
  DecisionId ->
  m Variant
deciderDecide _user deciderId dId =
  runExceptT (decideDecider deciderId dId) >>= \case
    Left e ->
      throwError $
        clientErrorAsServantError $ case e of
          DecideAggregateError aggE -> mapAggregateError (Just deciderId) aggE
          DecideWriteError we -> mapWriteError we
    Right v -> pure v

deciderDecisionSummarise ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  DeciderId ->
  DecisionId ->
  m Variant
deciderDecisionSummarise _user deciderId dId =
  runExceptT (summariseDecision deciderId dId) >>= \case
    Left e ->
      throwError $
        clientErrorAsServantError $ case e of
          DecisionSummaryAggregateError aggE -> mapAggregateError (Just deciderId) aggE
    Right v -> pure v

deciderRecordDecision ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  DeciderId ->
  DecisionId ->
  RecordDecisionRequest ->
  m NoContent
deciderRecordDecision _user deciderId dId recordDecReq =
  runExceptT
    ( recordDecision
        deciderId
        dId
        (getTyped @Variant recordDecReq)
        (getField @"decisionTime" recordDecReq)
    )
    >>= \case
      Left e ->
        throwError $
          clientErrorAsServantError $ case e of
            DecideAggregateError aggE -> mapAggregateError (Just deciderId) aggE
            DecideWriteError we -> mapWriteError we
      Right () -> pure NoContent

deciderDecisionInvalidate ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  DeciderId ->
  DecisionId ->
  InvalidateDecisionRequest ->
  m NoContent
deciderDecisionInvalidate _user deciderId dId invalidateReq =
  runExceptT (invalidateDecision deciderId dId (getTyped @_ invalidateReq)) >>= \case
    Left e ->
      throwError $
        clientErrorAsServantError $ case e of
          InvalidateDecisionAggregateError aggE -> mapAggregateError (Just deciderId) aggE
          InvalidateDecisionWriteError we -> mapWriteError we
    Right () -> pure NoContent

decidersSummarise ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  m [DeciderSummary]
decidersSummarise _user =
  getAllDeciderIds >>= mapM (deciderSummarise _user)

deciderRoutes :: User -> DeciderRoutes (AsServerT AppM)
deciderRoutes u =
  DeciderRoutes
    { _deciderCreate = deciderCreate u,
      _deciderSummarise = deciderSummarise u,
      _deciderDecide = deciderDecide u,
      _deciderDecisionSummarise = deciderDecisionSummarise u,
      _deciderRecordDecision = deciderRecordDecision u,
      _deciderDecisionInvalidate = deciderDecisionInvalidate u,
      _decidersSummarise = decidersSummarise u
    }
