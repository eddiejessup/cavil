module Cavil.Hashing where

import Cavil.Api.Decider
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Binary qualified as B
import Data.ByteString.Lazy qualified as BS.L
import Data.Generics.Product.Typed
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Protolude hiding ((%))

uuidFromArbitraryByteString :: BS.L.ByteString -> BS.L.ByteString -> UUID
uuidFromArbitraryByteString salt bs =
  B.decode @UUID $ BS.L.fromStrict $ SHA256.hashlazy (bs <> salt)

nextDecisionId :: Either DeciderId DecisionId -> DecisionId
nextDecisionId caseOrLastDecId =
  DecisionId $
    uuidFromArbitraryByteString decisionSalt $
      UUID.toByteString $ case caseOrLastDecId of
        Left caseId ->
          getTyped @UUID caseId
        Right lastId ->
          getTyped @UUID lastId
  where
    decisionSalt :: BS.L.ByteString
    decisionSalt = "OLQC1Q786FGq"
