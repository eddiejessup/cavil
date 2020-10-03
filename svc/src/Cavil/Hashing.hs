module Cavil.Hashing where

import Cavil.Api.Decider
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Binary qualified as B
import Data.ByteString.Lazy qualified as BS.L
import Data.Generics.Product.Typed
import Data.Generics.Sum.Typed
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Protolude hiding ((%))

uuidFromArbitraryByteString :: BS.L.ByteString -> BS.L.ByteString -> UUID
uuidFromArbitraryByteString salt bs =
  B.decode @UUID $ BS.L.fromStrict $ SHA256.hashlazy (bs <> salt)

nextUUIDInChain :: BS.L.ByteString -> UUID -> UUID
nextUUIDInChain salt a =
  uuidFromArbitraryByteString salt $ UUID.toByteString a

nextIdInChain :: (HasType UUID a, HasType UUID b, AsType UUID b) => BS.L.ByteString -> Either a b -> b
nextIdInChain salt decerOrLastDecId =
  injectTyped $ nextUUIDInChain salt $ case decerOrLastDecId of
    Left decerId ->
      getTyped @UUID decerId
    Right lastId ->
      getTyped @UUID lastId

nextDecisionIdInChain :: Either DeciderId DecisionId -> DecisionId
nextDecisionIdInChain = nextIdInChain "OLQC1Q786FGq"

nextFieldIdInChain :: Either DecisionId FieldId -> FieldId
nextFieldIdInChain = nextIdInChain "nYTAyHh7LZA0"
