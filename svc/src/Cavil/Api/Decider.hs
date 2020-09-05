{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Api.Decider where

import Control.Monad.Fail (MonadFail (fail))
import qualified Data.Aeson as Ae
import qualified Data.Time.Clock as T
import Data.UUID (UUID)
import Protolude
import Servant
import Servant.API.Generic

-- API.
data DeciderRoutes route = DeciderRoutes
  { _deciderCreate :: route :- ReqBody '[JSON] CreateDeciderRequest :> Put '[JSON] DeciderId,
    _deciderSummarise :: route :- Capture "deciderId" DeciderId :> Get '[JSON] DeciderSummary,
    _deciderDecide :: route :- Capture "deciderId" DeciderId :> Capture "decisionId" DecisionId :> Put '[JSON] Variant,
    _deciderDecisionInvalidate :: route :- Capture "deciderId" DeciderId :> Capture "decisionId" DecisionId :> "invalidate" :> ReqBody '[JSON] InvalidateDecisionRequest :> Post '[JSON] NoContent,
    _decidersSummarise :: route :- Get '[JSON] [DeciderSummary]
  }
  deriving stock (Generic)

-- /API.

-- Interface domain types.
newtype DeciderId = DeciderId {unDeciderId :: UUID}
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Ae.ToJSON, Ae.FromJSON, FromHttpApiData)

newtype Variant = Variant {unVariant :: Int}
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Ae.ToJSON)

instance Ae.FromJSON Variant where
  parseJSON value = do
    n <- Ae.parseJSON @Int value
    case mkVariant n of
      Nothing ->
        fail $ "parsing Variant failed, unexpected negative number: " <> show n
      Just v ->
        pure v

mkVariant :: Int -> Maybe Variant
mkVariant n
  | n < 0 = Nothing
  | otherwise = Just $ Variant n

newtype NrVariants = NrVariants {unNrVariants :: Int}
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON)

instance Ae.FromJSON NrVariants where
  parseJSON value = do
    n <- Ae.parseJSON @Int value
    case mkNrVariants n of
      Nothing ->
        fail $ "parsing NrVariants failed, unexpected number below 2: " <> show n
      Just v ->
        pure v

mkNrVariants :: Int -> Maybe NrVariants
mkNrVariants n
  | n < 2 = Nothing
  | otherwise = Just $ NrVariants n

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
    nrVariants :: NrVariants
  }
  deriving stock (Generic)
  deriving anyclass (Ae.FromJSON)

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
    nrVariants :: NrVariants,
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
