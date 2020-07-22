module Cavil.Serve.Common where

import Cavil.Api
import Cavil.Event
import Data.Aeson ((.=))
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Encoding as Ae.Enc
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.HashMap.Strict as HM
import qualified Database.PostgreSQL.Simple as PG
import Protolude hiding (Handler)
import Servant

-- Environment that handlers can read from.
data AppEnv = AppEnv
  { pgConn :: PG.Connection,
    clientUsername :: Text,
    clientPassword :: Text
  }
  deriving stock (Generic)

-- Monad that our app handlers run in.
type AppM = ReaderT AppEnv Handler

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
