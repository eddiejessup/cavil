module Cavil.Api.Ledger where

import Cavil.Api.Ledger.Var
import Control.Monad.Fail (MonadFail (fail))
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Ae
import Data.HashMap.Strict qualified as HM
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
  deriving newtype (Ae.ToJSON, Ae.ToJSONKey, Ae.FromJSON, Ae.FromJSONKey, Eq, FromHttpApiData)
  deriving newtype (Ord) -- For use in a Map

-- /Interface domain types.

-- Request interfaces.
data CreateLedgerRequest = CreateLedgerRequest
  { label :: LedgerLabel,
    schema :: PublicLedgerSchema
  }
  deriving stock (Generic)
  deriving anyclass (Ae.FromJSON)

type PublicLedgerSchema = Map FieldLabel FieldSchema

data FieldSchema
  = EnumFieldSchema VariantList
  | IntFieldSchema IntSchema

instance Ae.FromJSON FieldSchema where
  parseJSON = Ae.withObject "FieldSchema" $ \obj -> do
    fieldType <- obj .: "type"
    case fieldType :: Text of
      "enum" ->
        EnumFieldSchema <$> obj .: "variants"
      "int" -> do
        intSchema <-
          IntSchema
            <$> obj .: "min"
            <*> obj .: "max"
        pure $ IntFieldSchema intSchema

instance Ae.ToJSON FieldSchema where
  toJSON = \case
    EnumFieldSchema vs ->
      Ae.object ["type" .= Ae.String "enum", "variants" .= Ae.toJSON vs]
    IntFieldSchema intSc -> case Ae.toJSON intSc of
      Ae.Object obj ->
        Ae.Object $ HM.insert "type" (Ae.String "int") obj
      _ ->
        panic "IntSchema encoding did not return object"

data IntSchema = IntSchema {intMin, intMax :: Int}
  deriving stock (Generic)

instance Ae.FromJSON IntSchema where
  parseJSON = Ae.withObject "IntSchema" $ \obj -> do
    intMin <- obj .: "min"
    intMax <- obj .: "max"
    case mkIntSchema intMin intMax of
      Nothing ->
        fail $ "parsing IntSchema failed, max is not strictly greater than min: " <> show (intMin, intMax)
      Just v ->
        pure v

instance Ae.ToJSON IntSchema where
  toJSON IntSchema {intMin, intMax} =
    Ae.object ["min" .= intMin, "max" .= intMax]

mkIntSchema :: Int -> Int -> Maybe IntSchema
mkIntSchema intMin intMax
  | intMax > intMin = Just IntSchema {intMin, intMax}
  | otherwise = Nothing

type RecordEntryRequest = Map FieldLabel (FieldValSpec RawFieldValBody)

data RawFieldValBody
  = RawEnumBody Variant
  | RawIntBody Int

instance Ae.FromJSON RawFieldValBody where
  parseJSON v =
    RawEnumBody <$> Ae.parseJSON @Variant v
      <|> RawIntBody <$> Ae.parseJSON @Int v

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
type RecordEntryResponse = Map FieldLabel (FieldVal FieldValBody)

data FieldValBody
  = EnumBody VariantSelection
  | IntBody BoundedInt
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON)

data LedgerSummary = LedgerSummary
  { id :: LedgerId,
    nextEntryId :: EntryId,
    label :: LedgerLabel,
    schema :: PublicLedgerSchema,
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

type EntryBody = Map FieldLabel (FieldVal FieldValBody)

-- /Response interfaces.

-- Bounded Ints.

data BoundedInt = UnsafeBoundedInt Int
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON)

mkBoundedInt :: IntSchema -> Int -> Maybe BoundedInt
mkBoundedInt IntSchema {intMin, intMax} intVal
  | intVal >= intMin && intVal <= intMax = Just (UnsafeBoundedInt intVal)
  | otherwise = Nothing

-- / Bounded Ints.
