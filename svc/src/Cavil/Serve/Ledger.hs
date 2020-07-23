{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Serve.Ledger where

import Cavil.Api
import Cavil.Api.Common
import Cavil.Api.Ledger
import Cavil.Event.Common
import Cavil.Event.Ledger
import Cavil.Impl.Ledger
import Cavil.Serve.Common
import Data.Aeson ((.=))
import Data.Generics.Product.Typed
import Protolude
import Servant
import Servant.Server.Generic

data CreateLedgerError
  = CreateLedgerAggregateError (AggregateErrorWithState LedgerAggregateError)
  | CreateLedgerWriteError WriteError
  deriving stock (Generic)

data LedgerSummaryError
  = LedgerSummaryAggregateError (AggregateErrorWithState LedgerAggregateError)
  deriving stock (Generic)

data WriteRecordError
  = WriteRecordAggregateError (AggregateErrorWithState LedgerAggregateError)
  | WriteRecordWriteError WriteError
  deriving stock (Generic)

mapAggregateError :: LedgerLabel -> AggregateErrorWithState LedgerAggregateError -> ClientError
mapAggregateError ledgerLabel (AggregateErrorWithState aggState detail) =
  case aggState of
    AggregateBeforeRequest _ ->
      let detailMsg = case detail of
            LedgerAlreadyExists ->
              "Multiple ledger creation events"
            NoSuchLedger ->
              "Ledger events before ledger creation"
       in simpleClientError OurFault detailMsg ("ledgerLabel" .= ledgerLabel)
    AggregateDuringRequest _ ->
      let detailMsg = case detail of
            LedgerAlreadyExists ->
              "Ledger already exists"
            NoSuchLedger ->
              "No such ledger found"
       in simpleClientError BadRequest detailMsg ("ledgerLabel" .= ledgerLabel)

ledgerCreate ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  LedgerLabel ->
  m NoContent
ledgerCreate _user ledgerLabel =
  runExceptT (createLedger ledgerLabel) >>= \case
    Left e -> throwError $
      clientErrorAsServantError $ case e of
        CreateLedgerAggregateError aggE -> mapAggregateError ledgerLabel aggE
        CreateLedgerWriteError we -> mapWriteError we
    Right () ->
      pure NoContent

ledgerSummarise ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  LedgerLabel ->
  m LedgerSummary
ledgerSummarise _user ledgerLabel =
  runExceptT (summariseLedger ledgerLabel) >>= \case
    Left e -> throwError $
      clientErrorAsServantError $ case e of
        LedgerSummaryAggregateError aggE -> mapAggregateError ledgerLabel aggE
    Right v ->
      pure v

ledgerWrite ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  LedgerLabel ->
  LedgerWriteRequest ->
  m NoContent
ledgerWrite _user ledgerLabel wrReq =
  runExceptT (writeRecord ledgerLabel (getTyped @RecordTime wrReq) (getTyped @RecordBody wrReq)) >>= \case
    Left e ->
      throwError $
        clientErrorAsServantError $ case e of
          WriteRecordAggregateError aggE -> mapAggregateError ledgerLabel aggE
          WriteRecordWriteError we -> mapWriteError we
    Right () ->
      pure NoContent

ledgersSummarise ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  m [LedgerSummary]
ledgersSummarise _user =
  getAllLedgerLabels >>= mapM (ledgerSummarise _user)

ledgerRoutes :: User -> LedgerRoutes (AsServerT AppM)
ledgerRoutes u =
  LedgerRoutes
    { _ledgerCreate = ledgerCreate u,
      _ledgerSummarise = ledgerSummarise u,
      _ledgerWrite = ledgerWrite u,
      _ledgersSummarise = ledgersSummarise u
    }
