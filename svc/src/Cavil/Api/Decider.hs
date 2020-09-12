{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Api.Decider where

import Control.Monad.Fail (MonadFail (fail))
import Data.Aeson ((.:))
import Data.Aeson qualified as Ae
import Data.Time.Clock qualified as T
import Data.UUID (UUID)
import Protolude
import Servant
import Servant.API.Generic

-- API.

data DeciderRoutes route = DeciderRoutes
  { _deciderCreate :: route :- ReqBody '[JSON] CreateDeciderRequest :> Post '[JSON] DeciderId,
    _deciderSummarise :: route :- Capture "deciderId" DeciderId :> Get '[JSON] DeciderSummary,
    _deciderRecordDecision :: route :- Capture "deciderId" DeciderId :> Capture "decisionId" DecisionId :> ReqBody '[JSON] RecordDecisionRequest :> Put '[JSON] Variant,
    _deciderDecisionSummarise :: route :- Capture "deciderId" DeciderId :> Capture "decisionId" DecisionId :> Get '[JSON] Variant,
    _deciderDecisionInvalidate :: route :- Capture "deciderId" DeciderId :> Capture "decisionId" DecisionId :> "invalidate" :> ReqBody '[JSON] InvalidateDecisionRequest :> Post '[JSON] NoContent,
    _decidersSummarise :: route :- Get '[JSON] [DeciderSummary]
  }
  deriving stock (Generic)

-- /API.

-- Interface domain types.
newtype DeciderId = DeciderId {unDeciderId :: UUID}
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Ae.ToJSON, Ae.FromJSON, FromHttpApiData)

newtype Variant = Variant {unVariant :: Text}
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Ae.ToJSON, Ae.FromJSON, FromHttpApiData)

newtype VariantList = VariantList {unVariantList :: [Variant]}
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON)

instance Ae.FromJSON VariantList where
  parseJSON value = do
    vs <- Ae.parseJSON @[Variant] value
    case mkVariantList vs of
      Nothing ->
        fail $ "parsing VariantList failed, fewer than two variants: " <> show vs
      Just v ->
        pure v

mkVariantList :: [Variant] -> Maybe VariantList
mkVariantList vs
  | length vs < 2 = Nothing
  | otherwise = Just $ VariantList vs

newtype DecisionId = DecisionId {unDecisionId :: UUID}
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON, Ae.FromJSON, Eq, FromHttpApiData)
  deriving newtype (Ord) -- For use in a Map

newtype DeciderLabel = DeciderLabel {unDeciderLabel :: Text}
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON, Ae.FromJSON, FromHttpApiData)

-- /Interface domain types.

-- Request interfaces.
data CreateDeciderRequest = CreateDeciderRequest
  { label :: DeciderLabel,
    variants :: VariantList
  }
  deriving stock (Generic)
  deriving anyclass (Ae.FromJSON)

data RecordDecisionRequest = RecordDecisionRequest
  { variant :: DecisionVar Variant
  }
  deriving stock (Generic)
  deriving anyclass (Ae.FromJSON)

data DecisionVar a
  = RandomDecisionVar
  | ManualDecisionVar a T.UTCTime
  deriving stock (Generic)

instance Ae.FromJSON a => Ae.FromJSON (DecisionVar a) where
  parseJSON = \case
    Ae.Null -> pure RandomDecisionVar
    v ->
      let parse obj =
            ManualDecisionVar
              <$> obj .: "v"
              <*> obj .: "decisionTime"
       in Ae.withObject "ManualDecisionVar" parse v

data InvalidateDecisionRequest = InvalidateDecisionRequest
  { reason :: Text
  }
  deriving stock (Generic)
  deriving anyclass (Ae.FromJSON)

-- /Request interfaces.

-- Response interfaces.
data DeciderSummary = DeciderSummary
  { id :: DeciderId,
    nextDecisionId :: DecisionId,
    label :: DeciderLabel,
    variants :: VariantList,
    decisions :: [DecisionSummary]
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON)

data DecisionSummary = DecisionSummary
  { id :: DecisionId,
    decisionTime :: T.UTCTime,
    variant :: Variant,
    isValid :: Bool,
    invalidationReason :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON)

-- /Response interfaces.
