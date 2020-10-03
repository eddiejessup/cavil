module Cavil.Api.Ledger where

import Data.Aeson ((.:))
import Cavil.Api.Ledger.Var
import Data.Aeson qualified as Ae
import Data.Time.Clock qualified as T
import Data.UUID (UUID)
import Protolude
import Servant
import Servant.API.Generic

-- API.

data LedgerRoutes route = LedgerRoutes
  { _ledgerCreate :: route :- ReqBody '[JSON] CreateLedgerRequest :> Post '[JSON] LedgerId,
    _ledgerSummarise :: route :- Capture "ledgerId" LedgerId :> Get '[JSON] LedgerSummary,
    _ledgerRecordEntry :: route :- Capture "ledgerId" LedgerId :> Capture "entryId" EntryId :> ReqBody '[JSON] RecordEntryRequest :> Put '[JSON] RecordEntryResponse,
    _ledgerEntrySummarise :: route :- Capture "ledgerId" LedgerId :> Capture "entryId" EntryId :> Get '[JSON] EntrySummary,
    _ledgerEntryInvalidate :: route :- Capture "ledgerId" LedgerId :> Capture "entryId" EntryId :> "invalidate" :> ReqBody '[JSON] InvalidateEntryRequest :> Post '[JSON] NoContent,
    _ledgersSummarise :: route :- Get '[JSON] [LedgerSummary]
  }
  deriving stock (Generic)

-- /API.

-- Interface domain types.
newtype LedgerId = LedgerId {unLedgerId :: UUID}
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Ae.ToJSON, Ae.FromJSON, FromHttpApiData)

newtype LedgerLabel = LedgerLabel {unLedgerLabel :: Text}
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON, Ae.FromJSON, FromHttpApiData)

newtype EntryId = EntryId {unEntryId :: UUID}
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON, Ae.FromJSON, Eq, FromHttpApiData)
  deriving newtype (Ord) -- For use in a Map

newtype FieldLabel = FieldLabel {unFieldLabel :: Text}
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Ae.ToJSONKey, Ae.FromJSONKey, FromHttpApiData, ToHttpApiData)

newtype FieldId = FieldId {unFieldId :: UUID}
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON, Ae.FromJSON, Eq, FromHttpApiData)
  deriving newtype (Ord) -- For use in a Map

-- /Interface domain types.

-- Request interfaces.
data CreateLedgerRequest = CreateLedgerRequest
  { label :: LedgerLabel,
    schema :: LedgerSchema
  }
  deriving stock (Generic)
  deriving anyclass (Ae.FromJSON)

type LedgerSchema = Map FieldLabel VariantList

type RecordEntryRequest = Map FieldLabel (FieldValSpec RawVariantSelection)

type RecordEntryResponse = Map FieldLabel (FieldVal VariantSelection)

data InvalidateEntryRequest = InvalidateEntryRequest
  { reason :: Text
  }
  deriving stock (Generic)
  deriving anyclass (Ae.FromJSON)

data FieldValSpec a
  = RandomFieldValSpec
  | ManualFieldValSpec (FieldVal a)
  deriving stock (Generic)

instance Ae.FromJSON a => Ae.FromJSON (FieldValSpec a) where
  parseJSON = \case
    Ae.Null -> pure RandomFieldValSpec
    v -> ManualFieldValSpec <$> Ae.parseJSON v

data FieldVal a = FieldVal
  { v :: a,
    entryTime :: T.UTCTime
  }
  deriving stock (Generic, Functor)
  deriving anyclass (Ae.ToJSON)

-- Mimics API of 'distributive', but not worth adding it as a dependency.
distributeMaybeFieldVal :: FieldVal (Maybe a) -> Maybe (FieldVal a)
distributeMaybeFieldVal FieldVal {v = Nothing} = Nothing
distributeMaybeFieldVal FieldVal {v = Just a, entryTime} = Just FieldVal {v = a, entryTime}

collectMaybeFieldVal :: (a -> Maybe b) -> FieldVal a -> Maybe (FieldVal b)
collectMaybeFieldVal f = distributeMaybeFieldVal . fmap f

-- /Mimics API of 'distributive'

instance Ae.FromJSON a => Ae.FromJSON (FieldVal a) where
  parseJSON =
    Ae.withObject "FieldVal" $ \obj ->
      FieldVal
        <$> obj .: "v"
        <*> obj .: "entryTime"

-- /Request interfaces.

-- Response interfaces.
data LedgerSummary = LedgerSummary
  { id :: LedgerId,
    nextEntryId :: EntryId,
    label :: LedgerLabel,
    schema :: LedgerSchema,
    entries :: [EntrySummary]
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON)

data EntrySummary = EntrySummary
  { id :: EntryId,
    creationTime :: T.UTCTime,
    body :: EntryBody,
    isValid :: Bool,
    invalidationReason :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON)

type EntryBody = Map FieldLabel (FieldVal VariantSelection)

-- /Response interfaces.
