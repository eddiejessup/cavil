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

data RecordEntryError
  = RecordEntryAggregateError (AggregateErrorWithState LedgerAggregateError)
  | RecordEntryWriteError WriteError
  deriving stock (Generic)

data EntrySummaryError
  = EntrySummaryAggregateError (AggregateErrorWithState LedgerAggregateError)
  deriving stock (Generic)

data InvalidateEntryError
  = InvalidateEntryAggregateError (AggregateErrorWithState LedgerAggregateError)
  | InvalidateEntryWriteError WriteError
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
            NoSuchEntry ->
              "Entry events before entry creation"
            NoSuchVariant ->
              "Unknown variants in entry chain"
            IncoherentEntryId ->
              "Incoherent IDs in entry chain"
            EntryAlreadyInvalidated ->
              "Multiple invalidations of entry"
            InconsistentVariant ->
              "Ambiguous entry-variant"
       in simpleClientError OurFault detailMsg vals
    AggregateDuringRequest _ ->
      let detailMsg = case detail of
            LedgerAlreadyExists ->
              "Ledger already exists"
            NoSuchLedger ->
              "No such ledger found"
            NoSuchEntry ->
              "No such entry found"
            NoSuchVariant ->
              "Variant not an option"
            IncoherentEntryId ->
              "Incoherent entry ID"
            EntryAlreadyInvalidated ->
              "Entry has already been invalidated"
            InconsistentVariant ->
              "Manual entry-variant is inconsistent with existing variant"
       in simpleClientError BadRequest detailMsg vals
  where
    vals = maybe mempty ("ledgerId" .=) mayLedgerId

ledgerCreate ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  CreateLedgerRequest ->
  m LedgerId
ledgerCreate _user ccReq =
  runExceptT (createLedger (getTyped @LedgerLabel ccReq) (getField @"schema" ccReq)) >>= \case
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

ledgerEntrySummarise ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  LedgerId ->
  EntryId ->
  m EntrySummary
ledgerEntrySummarise _user ledgerId dId =
  runExceptT (summariseEntry ledgerId dId) >>= \case
    Left e ->
      throwError $
        clientErrorAsServantError $ case e of
          EntrySummaryAggregateError aggE -> mapAggregateError (Just ledgerId) aggE
    Right v -> pure v

ledgerRecordEntry ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  LedgerId ->
  EntryId ->
  RecordEntryRequest ->
  m RecordEntryResponse
ledgerRecordEntry _user ledgerId dId recordDecReq =
  runExceptT (recordEntry ledgerId dId recordDecReq)
    >>= \case
      Left e ->
        throwError $
          clientErrorAsServantError $ case e of
            RecordEntryAggregateError aggE -> mapAggregateError (Just ledgerId) aggE
            RecordEntryWriteError we -> mapWriteError we
      Right v -> pure v

ledgerEntryInvalidate ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  User ->
  LedgerId ->
  EntryId ->
  InvalidateEntryRequest ->
  m NoContent
ledgerEntryInvalidate _user ledgerId dId invalidateReq =
  runExceptT (invalidateEntry ledgerId dId (getTyped @_ invalidateReq)) >>= \case
    Left e ->
      throwError $
        clientErrorAsServantError $ case e of
          InvalidateEntryAggregateError aggE -> mapAggregateError (Just ledgerId) aggE
          InvalidateEntryWriteError we -> mapWriteError we
    Right () -> pure NoContent

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
      _ledgerEntrySummarise = ledgerEntrySummarise u,
      _ledgerRecordEntry = ledgerRecordEntry u,
      _ledgerEntryInvalidate = ledgerEntryInvalidate u,
      _ledgersSummarise = ledgersSummarise u
    }
