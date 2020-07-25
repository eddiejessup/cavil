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

mapAggregateError :: Maybe LedgerId -> AggregateErrorWithState LedgerAggregateError -> ClientError
mapAggregateError mayLedgerId (AggregateErrorWithState aggState detail) =
  case aggState of
    AggregateBeforeRequest _ ->
      let detailMsg = case detail of
            LedgerAlreadyExists ->
              "Multiple ledger creation events"
            NoSuchLedger ->
              "Ledger events before ledger creation"
       in simpleClientError OurFault detailMsg vals
    AggregateDuringRequest _ ->
      let detailMsg = case detail of
            LedgerAlreadyExists ->
              "Ledger already exists"
            NoSuchLedger ->
              "No such ledger found"
       in simpleClientError BadRequest detailMsg vals
  where
    vals = maybe mempty ("ledgerId" .=) mayLedgerId

ledgerCreate ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  CreateLedgerRequest ->
  m LedgerId
ledgerCreate _user lcReq =
  runExceptT (createLedger (getTyped @LedgerLabel lcReq)) >>= \case
    Left e -> throwError $
      clientErrorAsServantError $ case e of
        CreateLedgerAggregateError aggE -> mapAggregateError Nothing aggE
        CreateLedgerWriteError we -> mapWriteError we
    Right v ->
      pure v

ledgerSummarise ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  LedgerId ->
  m LedgerSummary
ledgerSummarise _user ledgerId =
  runExceptT (summariseLedger ledgerId) >>= \case
    Left e -> throwError $
      clientErrorAsServantError $ case e of
        LedgerSummaryAggregateError aggE -> mapAggregateError (Just ledgerId) aggE
    Right v ->
      pure v

ledgerWrite ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  LedgerId ->
  LedgerWriteRequest ->
  m RecordId
ledgerWrite _user ledgerId wrReq =
  runExceptT (writeRecord ledgerId (getTyped @RecordTime wrReq) (getTyped @RecordBody wrReq)) >>= \case
    Left e ->
      throwError $
        clientErrorAsServantError $ case e of
          WriteRecordAggregateError aggE -> mapAggregateError (Just ledgerId) aggE
          WriteRecordWriteError we -> mapWriteError we
    Right v ->
      pure v

ledgersSummarise ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  m [LedgerSummary]
ledgersSummarise _user =
  getAllLedgerIds >>= mapM (ledgerSummarise _user)

ledgerRoutes :: User -> LedgerRoutes (AsServerT AppM)
ledgerRoutes u =
  LedgerRoutes
    { _ledgerCreate = ledgerCreate u,
      _ledgerSummarise = ledgerSummarise u,
      _ledgerWrite = ledgerWrite u,
      _ledgersSummarise = ledgersSummarise u
    }
