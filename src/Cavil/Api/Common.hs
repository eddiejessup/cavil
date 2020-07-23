{-# LANGUAGE DuplicateRecordFields #-}

module Cavil.Api.Common where

import qualified Data.Aeson as Ae
import qualified Data.HashMap.Strict as HM
import Protolude

-- Errors.

data ClientError = ClientError ClientErrorReason (HM.HashMap Text Ae.Value)
  deriving stock (Generic)

instance Ae.ToJSON ClientError where
  toJSON (ClientError reason vals) =
    Ae.Object $ HM.insert "reason" (Ae.toJSON reason) vals

data ClientErrorReason
  = OurFault
  | BadRequest
  deriving stock (Generic)

instance Ae.ToJSON ClientErrorReason where
  toJSON = Ae.String . renderClientReason

renderClientReason :: IsString s => ClientErrorReason -> s
renderClientReason = \case
  OurFault -> "ourFault"
  BadRequest -> "badRequest"

-- /Errors.
