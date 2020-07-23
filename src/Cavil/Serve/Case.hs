{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Serve.Case where

import Cavil.Api
import Cavil.Api.Common
import Cavil.Api.Case
import Cavil.Event.Common
import Cavil.Event.Case
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

mapAggregateError :: AggregateErrorWithState CaseAggregateError -> ClientError
mapAggregateError (AggregateErrorWithState aggState detail) =
  case aggState of
    AggregateBeforeRequest _ ->
      let detailMsg = case detail of
            CaseAlreadyExists ->
              "Multiple case creation events"
            NoSuchCase ->
              "Case events before case creation"
            NoSuchDecision ->
              "Decision events before decision creation"
            IncoherentDecisionToken ->
              "Incoherent tokens in decision chain"
            DecisionAlreadyInvalidated ->
              "Multiple invalidations of decision"
       in ClientError OurFault $
            mconcat
              [ "errorType" .= ("Invalid existing data" :: Text),
                "errorDetail" .= (detailMsg :: Text)
              ]
    AggregateDuringRequest _ ->
      let detailMsg = case detail of
            CaseAlreadyExists ->
              "Case already exists"
            NoSuchCase ->
              "No such case found"
            NoSuchDecision ->
              "No such decision found"
            IncoherentDecisionToken ->
              "Incoherent decision token"
            DecisionAlreadyInvalidated ->
              "Decision has already been invalidated"
       in ClientError BadRequest $
            mconcat
              [ "errorType" .= ("Bad request" :: Text),
                "errorDetail" .= (detailMsg :: Text)
              ]

caseCreate ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  CaseLabel ->
  CreateCaseRequest ->
  m NoContent
caseCreate _user caseLabel ccReq =
  runExceptT (createCase caseLabel (getTyped @NrVariants ccReq)) >>= \case
    Left e -> throwError $
      clientErrorAsServantError $ case e of
        CreateCaseAggregateError aggE -> mapAggregateError aggE
        CreateCaseWriteError we -> mapWriteError we
    Right () ->
      pure NoContent

caseSummarise ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  CaseLabel ->
  m CaseSummary
caseSummarise _user caseLabel =
  runExceptT (summariseCase caseLabel) >>= \case
    Left e -> throwError $
      clientErrorAsServantError $ case e of
        CaseSummaryAggregateError aggE -> mapAggregateError aggE
    Right v ->
      pure v

caseDecide ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  CaseLabel ->
  DecisionToken ->
  m Variant
caseDecide _user caseLabel tok =
  runExceptT (decideCase caseLabel tok) >>= \case
    Left e ->
      throwError $
        clientErrorAsServantError $ case e of
          DecideAggregateError aggE -> mapAggregateError aggE
          DecideWriteError we -> mapWriteError we
    Right v -> pure v

caseDecisionInvalidate ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  CaseLabel ->
  DecisionToken ->
  InvalidateDecisionRequest ->
  m NoContent
caseDecisionInvalidate _user caseLabel tok invalidateReq =
  runExceptT (invalidateDecision caseLabel tok (getField @"reason" invalidateReq)) >>= \case
    Left e ->
      throwError $
        clientErrorAsServantError $ case e of
          InvalidateDecisionAggregateError aggE -> mapAggregateError aggE
          InvalidateDecisionWriteError we -> mapWriteError we
    Right () -> pure NoContent

casesSummarise ::
  (MonadIO m, MonadReader AppEnv m) =>
  User ->
  m [CaseSummariesItem]
casesSummarise _user =
  getAllCaseLabels >>= mapM \caseLabel ->
    runExceptT (summariseCase caseLabel) <&> \case
      Left (CaseSummaryAggregateError aggE) ->
        FailedCaseSummariesItem $
          FailedCaseSummary
            { label = caseLabel,
              error = mapAggregateError aggE
            }
      Right v ->
        SucceededCaseSummariesItem v

caseRoutes :: User -> CaseRoutes (AsServerT AppM)
caseRoutes u =
  CaseRoutes
    { _caseCreate = caseCreate u,
      _caseSummarise = caseSummarise u,
      _caseDecide = caseDecide u,
      _caseDecisionInvalidate = caseDecisionInvalidate u,
      _casesSummarise = casesSummarise u
    }
