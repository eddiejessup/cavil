{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Serve.Case where

import Cavil.Api
import Cavil.Api.Case
import Cavil.Api.Common
import Cavil.Event.Case
import Cavil.Event.Common
import Cavil.Impl.Case
import Cavil.Serve.Common
import Data.Aeson ((.=))
import Data.Generics.Product.Typed
import Protolude
import Servant
import Servant.Server.Generic

data CreateCaseError
  = CreateCaseAggregateError (AggregateErrorWithState CaseAggregateError)
  | CreateCaseWriteError WriteError
  deriving stock (Generic)

data CaseSummaryError
  = CaseSummaryAggregateError (AggregateErrorWithState CaseAggregateError)
  deriving stock (Generic)

data DecideError
  = DecideAggregateError (AggregateErrorWithState CaseAggregateError)
  | DecideWriteError WriteError
  deriving stock (Generic)

data InvalidateDecisionError
  = InvalidateDecisionAggregateError (AggregateErrorWithState CaseAggregateError)
  | InvalidateDecisionWriteError WriteError
  deriving stock (Generic)

mapAggregateError :: Maybe CaseId -> AggregateErrorWithState CaseAggregateError -> ClientError
mapAggregateError mayCaseId (AggregateErrorWithState aggState detail) =
  case aggState of
    AggregateBeforeRequest _ ->
      let detailMsg = case detail of
            CaseAlreadyExists ->
              "Multiple case creation events"
            NoSuchCase ->
              "Case events before case creation"
            NoSuchDecision ->
              "Decision events before decision creation"
            IncoherentDecisionId ->
              "Incoherent IDs in decision chain"
            DecisionAlreadyInvalidated ->
              "Multiple invalidations of decision"
       in simpleClientError OurFault detailMsg vals
    AggregateDuringRequest _ ->
      let detailMsg = case detail of
            CaseAlreadyExists ->
              "Case already exists"
            NoSuchCase ->
              "No such case found"
            NoSuchDecision ->
              "No such decision found"
            IncoherentDecisionId ->
              "Incoherent decision ID"
            DecisionAlreadyInvalidated ->
              "Decision has already been invalidated"
       in simpleClientError OurFault detailMsg vals
  where
    vals = maybe mempty ("caseId" .=) mayCaseId

caseCreate ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  CreateCaseRequest ->
  m CaseId
caseCreate _user ccReq =
  runExceptT (createCase (getTyped @CaseLabel ccReq) (getTyped @NrVariants ccReq)) >>= \case
    Left e -> throwError $
      clientErrorAsServantError $ case e of
        CreateCaseAggregateError aggE -> mapAggregateError Nothing aggE
        CreateCaseWriteError we -> mapWriteError we
    Right v ->
      pure v

caseSummarise ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  CaseId ->
  m CaseSummary
caseSummarise _user caseId =
  runExceptT (summariseCase caseId) >>= \case
    Left e -> throwError $
      clientErrorAsServantError $ case e of
        CaseSummaryAggregateError aggE -> mapAggregateError (Just caseId) aggE
    Right v ->
      pure v

caseDecide ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  CaseId ->
  DecisionId ->
  m Variant
caseDecide _user caseId dId =
  runExceptT (decideCase caseId dId) >>= \case
    Left e ->
      throwError $
        clientErrorAsServantError $ case e of
          DecideAggregateError aggE -> mapAggregateError (Just caseId) aggE
          DecideWriteError we -> mapWriteError we
    Right v -> pure v

caseDecisionInvalidate ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  CaseId ->
  DecisionId ->
  InvalidateDecisionRequest ->
  m NoContent
caseDecisionInvalidate _user caseId dId invalidateReq =
  runExceptT (invalidateDecision caseId dId (getField @"reason" invalidateReq)) >>= \case
    Left e ->
      throwError $
        clientErrorAsServantError $ case e of
          InvalidateDecisionAggregateError aggE -> mapAggregateError (Just caseId) aggE
          InvalidateDecisionWriteError we -> mapWriteError we
    Right () -> pure NoContent

casesSummarise ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  m [CaseSummary]
casesSummarise _user =
  getAllCaseIds >>= mapM (caseSummarise _user)

caseRoutes :: User -> CaseRoutes (AsServerT AppM)
caseRoutes u =
  CaseRoutes
    { _caseCreate = caseCreate u,
      _caseSummarise = caseSummarise u,
      _caseDecide = caseDecide u,
      _caseDecisionInvalidate = caseDecisionInvalidate u,
      _casesSummarise = casesSummarise u
    }
