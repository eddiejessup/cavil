{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Hashing where

import Cavil.Api.Case
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS.L
import Data.Generics.Product.Typed
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Protolude hiding ((%))

uuidFromArbitraryByteString :: BS.L.ByteString -> BS.L.ByteString -> UUID
uuidFromArbitraryByteString salt bs =
  B.decode @UUID $ BS.L.fromStrict $ SHA256.hashlazy (bs <> salt)

nextDecisionToken :: Either CaseLabel DecisionToken -> DecisionToken
nextDecisionToken labOrLastTok =
  DecisionToken $
    uuidFromArbitraryByteString decisionSalt $ case labOrLastTok of
      Left caseLabel ->
        B.encode (getTyped @Text caseLabel)
      Right lastTok ->
        UUID.toByteString $ getTyped @UUID lastTok
  where
    decisionSalt :: BS.L.ByteString
    decisionSalt = "OLQC1Q786FGq"
