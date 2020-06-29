{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Cavil.Serve where

import Cavil.Api
import Cavil.Event
import Cavil.Impl
import Data.Generics.Product.Typed
import qualified Database.PostgreSQL.Simple as PG
import Protolude hiding ((%), Handler)
import Servant
import Servant.Server.Generic
import Data.Aeson
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Lazy as BS.L

data CreateCaseError
  = CreateCaseAggregateError AggregateError
  | CreateCaseWriteError WriteError
  | CreateCaseDomainError CreateCaseDomainError
  deriving stock (Generic)

data CaseSummaryError
  = CaseSummaryAggregateError AggregateError
  deriving stock (Generic)

data DecideError
  = DecideAggregateError AggregateError
  | DecideWriteError WriteError
  | DecideDomainError DecideDomainError
  deriving stock (Generic)

bsFromPairs :: [Series] -> BS.L.ByteString
bsFromPairs = B.toLazyByteString . fromEncoding . pairs . mconcat

mapAggregateError :: AggregateError -> ServerError
mapAggregateError (AggregateError aggState detail) =
  case aggState of
    AggregateBeforeRequest _ ->
      let
        detailMsg = case detail of
          CaseAlreadyExists ->
            "Multiple case creation events"
          NoSuchCase ->
            "Case events before case creation"
          IncoherentDecisionToken ->
            "Incoherent decision token events in chain"
      in
        err500
          { errBody = bsFromPairs
              [ "errorType" .= ("Invalid existing data" :: Text)
              , "errorDetail" .= (detailMsg :: Text)
              ]
          }
    AggregateDuringRequest _ ->
      let
        detailMsg = case detail of
          CaseAlreadyExists ->
            "Case already exists"
          NoSuchCase ->
            "No such case found"
          IncoherentDecisionToken ->
            "Incoherent decision token"
      in
        err400
          { errBody = bsFromPairs
              [ "errorType" .= ("Bad request" :: Text)
              , "errorDetail" .= (detailMsg :: Text)
              ]
          }

mapWriteError :: WriteError -> ServerError
mapWriteError e =
  let
    detailMsg = case e of
      InsertedUnexpectedNrRows _ ->
        "Inserted unexpected number of rows"
  in
    err500
      { errBody = bsFromPairs
          [ "errorType" .= ("Bad write" :: Text)
          , "errorDetail" .= (detailMsg :: Text)
          ]
      }

createHandler ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  CaseLabel ->
  CreateCaseRequest ->
  m NoContent
createHandler caseLabel ccReq =
  runExceptT (createCase caseLabel (getTyped @NrVariants ccReq)) >>= \case
    Left e -> throwError $ case e of
      CreateCaseAggregateError aggE -> mapAggregateError aggE
      CreateCaseWriteError we -> mapWriteError we
    Right () ->
      pure NoContent

summariseCaseHandler ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  CaseLabel ->
  m CaseSummary
summariseCaseHandler caseLabel =
  runExceptT (summariseCase caseLabel) >>= \case
    Left e -> throwError $ case e of
      CaseSummaryAggregateError aggE -> mapAggregateError aggE
    Right v ->
      pure v

decideHandler ::
  (MonadIO m, MonadReader AppEnv m, MonadError ServerError m) =>
  CaseLabel ->
  DecisionRequest ->
  m Variant
decideHandler caseLabel dReq =
  runExceptT (decide caseLabel (getTyped @DecisionToken dReq)) >>= \case
    Left e ->
      throwError $ case e of
        DecideAggregateError aggE -> mapAggregateError aggE
        DecideWriteError we -> mapWriteError we
    Right v -> pure v

record :: Routes (AsServerT AppM)
record =
  Routes
    { _create = createHandler,
      _summarise = summariseCaseHandler,
      _decide = decideHandler
    }

-- Environment that handlers can read from.
data AppEnv = AppEnv
  { pgConn :: PG.Connection
  }
  deriving stock Generic

-- Monad that our app handlers run in.
type AppM = ReaderT AppEnv Handler

-- Natural transformation from our custom handler monad to the servant monad.
appToHandler :: AppEnv -> AppM a -> Handler a
appToHandler env m = runReaderT m env

app :: AppEnv -> Application
app env = genericServeT (appToHandler env) record
