module Cavil.Hashing where

import Cavil.Api.Ledger
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
nextIdInChain salt initOrLastId =
  injectTyped $ nextUUIDInChain salt $ case initOrLastId of
    Left ledgerId ->
      getTyped @UUID ledgerId
    Right lastId ->
      getTyped @UUID lastId

nextEntryIdInChain :: Either LedgerId EntryId -> EntryId
nextEntryIdInChain = nextIdInChain "OLQC1Q786FGq"

nextFieldIdInChain :: Either EntryId FieldId -> FieldId
nextFieldIdInChain = nextIdInChain "nYTAyHh7LZA0"
