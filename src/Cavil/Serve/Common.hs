module Cavil.Serve.Common where

import Cavil.Api.Common
import Cavil.Event.Common
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

simpleClientError :: ClientErrorReason -> Text -> HM.HashMap Text Ae.Value -> ClientError
simpleClientError reason detail vals =
  ClientError reason $
    mconcat
      [ "errorType" .= errType,
        "errorDetail" .= detail,
        vals
      ]
  where
    errType :: Text
    errType = case reason of
      OurFault -> "Invalid existing data"
      BadRequest -> "Bad request"
