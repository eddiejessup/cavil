{-# LANGUAGE DuplicateRecordFields #-}

module Cavil.Api.Ledger where

import qualified Data.Aeson as Ae
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Protolude
import Servant
import Servant.API.Generic

-- API.

data LedgerRoutes route = LedgerRoutes
  { _ledgerCreate :: route :- ReqBody '[JSON] CreateLedgerRequest :> Put '[JSON] LedgerId,
    _ledgerSummarise :: route :- Capture "ledgerId" LedgerId :> Get '[JSON] LedgerSummary,
    _ledgerWrite :: route :- Capture "ledgerId" LedgerId :> ReqBody '[JSON] LedgerWriteRequest :> Put '[JSON] RecordId,
    _ledgersSummarise :: route :- Get '[JSON] [LedgerSummary]
  }
  deriving stock (Generic)

-- /API.

-- Interface domain types.
newtype LedgerId = LedgerId {unLedgerId :: UUID}
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Ae.ToJSON, Ae.FromJSON, FromHttpApiData)

newtype RecordId = RecordId {unRecordId :: UUID}
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Ae.ToJSON, Ae.FromJSON, FromHttpApiData)

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

data CreateLedgerRequest = CreateLedgerRequest
  { label :: LedgerLabel
  }
  deriving stock (Generic)
  deriving anyclass (Ae.FromJSON)

data LedgerWriteRequest = LedgerWriteRequest
  { recordTime :: RecordTime,
    body :: RecordBody
  }
  deriving stock (Generic)
  deriving anyclass (Ae.FromJSON)

-- /Request interfaces.

-- Response interfaces.

data LedgerSummary = LedgerSummary
  { id :: LedgerId,
    label :: LedgerLabel,
    records :: [RecordSummary]
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON)

data RecordSummary = RecordSummary
  { id :: RecordId,
    recordTime :: RecordTime,
    body :: RecordBody
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON)

-- /Response interfaces.
