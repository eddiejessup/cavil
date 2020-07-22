{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cavil.Serve where

import Cavil.Api
import Cavil.Event
import Cavil.Impl
import Data.Aeson ((.=))
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Encoding as Ae.Enc
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Lazy as BS.L
import Data.Generics.Product.Typed
import qualified Data.HashMap.Strict as HM
import qualified Database.PostgreSQL.Simple as PG
import Protolude hiding (Handler, (%))
import Servant
import Servant.Server.Generic

data CreateCaseError
  = CreateCaseAggregateError AggregateError
  | CreateCaseWriteError WriteError
  deriving stock (Generic)

data CaseSummaryError
  = CaseSummaryAggregateError AggregateError
  deriving stock (Generic)

data DecideError
  = DecideAggregateError AggregateError
  | DecideWriteError WriteError
  deriving stock (Generic)

data InvalidateDecisionError
  = InvalidateDecisionAggregateError AggregateError
  | InvalidateDecisionWriteError WriteError
  deriving stock (Generic)

bsFromAeObject :: HM.HashMap Text Ae.Value -> BS.L.ByteString
bsFromAeObject = B.toLazyByteString . Ae.fromEncoding . Ae.Enc.value . Ae.Object

mapAggregateError :: AggregateError -> ClientError
mapAggregateError (AggregateError aggState detail) =
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

mapWriteError :: WriteError -> ClientError
mapWriteError e =
  let detailMsg = case e of
        InsertedUnexpectedNrRows _ ->
          "Inserted unexpected number of rows"
   in ClientError OurFault $
        mconcat
          [ "errorType" .= ("Bad write" :: Text),
            "errorDetail" .= (detailMsg :: Text)
          ]

clientErrorAsServantError :: ClientError -> ServerError
clientErrorAsServantError (ClientError reason msg) =
  let respType = case reason of
        OurFault -> err500
        BadRequest -> err400
   in respType {errBody = bsFromAeObject msg}

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

record :: Routes (AsServerT AppM)
record =
  Routes
    { _caseCreate = caseCreate,
      _caseSummarise = caseSummarise,
      _caseDecide = caseDecide,
      _caseDecisionInvalidate = caseDecisionInvalidate,
      _casesSummarise = casesSummarise
    }

-- Environment that handlers can read from.
data AppEnv = AppEnv
  { pgConn :: PG.Connection,
    clientUsername :: Text,
    clientPassword :: Text
  }
  deriving stock (Generic)

-- Monad that our app handlers run in.
type AppM = ReaderT AppEnv Handler

-- Natural transformation from our custom handler monad to the servant monad.
appToHandler :: AppEnv -> AppM a -> Handler a
appToHandler env m = runReaderT m env

mkWebApplication :: AppEnv -> Application
mkWebApplication env = genericServeTWithContext (appToHandler env) record ctx
  where
    ctx = checkBasicAuth env :. EmptyContext

checkBasicAuth :: AppEnv -> BasicAuthCheck User
checkBasicAuth env = BasicAuthCheck \basicAuthData ->
  let username = decodeUtf8 $ basicAuthUsername basicAuthData
      password = decodeUtf8 $ basicAuthPassword basicAuthData
   in pure $
        if username == clientUsername env
          then
            if password == clientPassword env
              then Authorized ()
              else BadPassword
          else NoSuchUser
