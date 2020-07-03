{-# LANGUAGE TypeApplications #-}

module Cavil.Hashing where

import Cavil.Api
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS.L
import Data.Generics.Product.Typed
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Protolude hiding ((%), to)

uuidFromArbitraryByteString :: BS.L.ByteString -> UUID
uuidFromArbitraryByteString bs =
  B.decode @UUID $ BS.L.fromStrict $ SHA256.hashlazy bs

uuidFromArbitraryText :: Text -> UUID
uuidFromArbitraryText = uuidFromArbitraryByteString . B.encode

pickVariant :: DecisionToken -> NrVariants -> Variant
pickVariant decisionToken nrVariants =
  let (w1, w2, w3, w4) = UUID.toWords $ getTyped @UUID decisionToken
      nonModuloInt = abs $ sum $ fromIntegral @Word32 @Int <$> [w1, w2, w3, w4]
      moduloInt = nonModuloInt `mod` getTyped @Int nrVariants
   in case mkVariant moduloInt of
        Nothing -> panic "impossible"
        Just v -> v

initialDecisionSalt :: Text
initialDecisionSalt = "OLQC1Q786FGq"
