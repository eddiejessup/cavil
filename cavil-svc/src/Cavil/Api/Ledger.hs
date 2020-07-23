{-# LANGUAGE DuplicateRecordFields #-}

module Cavil.Api.Ledger where

import qualified Data.Aeson as Ae
import Data.Time.Clock (UTCTime)
import Protolude
import Servant
import Servant.API.Generic

-- API.

data LedgerRoutes route = LedgerRoutes
  { _ledgerCreate :: route :- Capture "label" LedgerLabel :> Put '[JSON] NoContent,
    _ledgerSummarise :: route :- Capture "label" LedgerLabel :> Get '[JSON] LedgerSummary,
    _ledgerWrite :: route :- Capture "label" LedgerLabel :> ReqBody '[JSON] LedgerWriteRequest :> Put '[JSON] NoContent,
    _ledgersSummarise :: route :- Get '[JSON] [LedgerSummary]
  }
  deriving stock (Generic)

-- /API.

-- Interface domain types.

newtype RecordTime = RecordTime {unRecordTime :: UTCTime}
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON, Ae.FromJSON, Eq, Ord)

newtype LedgerLabel = LedgerLabel {caseLabelText :: Text}
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON, Ae.FromJSON, FromHttpApiData)

newtype RecordBody = RecordBody {unRecordBody :: Text}
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON, Ae.FromJSON)

-- /Interface domain types.

-- Request interfaces.

data LedgerWriteRequest = LedgerWriteRequest
  { recordTime :: RecordTime,
    body :: RecordBody
  }
  deriving stock (Generic)
  deriving anyclass (Ae.FromJSON)

-- /Request interfaces.

-- Response interfaces.

data LedgerSummary = LedgerSummary
  { label :: LedgerLabel,
    records :: [RecordSummary]
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON)

data RecordSummary = RecordSummary
  { recordTime :: RecordTime,
    body :: RecordBody
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON)

-- /Response interfaces.
