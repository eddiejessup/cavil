{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Api.Case where

import Control.Monad.Fail (MonadFail (fail))
import qualified Data.Aeson as Ae
import qualified Data.Time.Clock as T
import Data.UUID (UUID)
import Protolude
import Servant
import Servant.API.Generic

-- API.
data CaseRoutes route = CaseRoutes
  { _caseCreate :: route :- ReqBody '[JSON] CreateCaseRequest :> Put '[JSON] CaseId,
    _caseSummarise :: route :- Capture "caseId" CaseId :> Get '[JSON] CaseSummary,
    _caseDecide :: route :- Capture "caseId" CaseId :> Capture "decisionId" DecisionId :> Put '[JSON] Variant,
    _caseDecisionInvalidate :: route :- Capture "caseId" CaseId :> Capture "decisionId" DecisionId :> "invalidate" :> ReqBody '[JSON] InvalidateDecisionRequest :> Post '[JSON] NoContent,
    _casesSummarise :: route :- Get '[JSON] [CaseSummary]
  }
  deriving stock (Generic)

-- /API.

-- Interface domain types.
newtype CaseId = CaseId {unCaseId :: UUID}
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

newtype CaseLabel = CaseLabel {unCaseLabel :: Text}
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON, Ae.FromJSON, FromHttpApiData)

-- /Interface domain types.

-- Request interfaces.
data CreateCaseRequest = CreateCaseRequest
  { label :: CaseLabel,
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
data CaseSummary = CaseSummary
  { id :: CaseId,
    nextDecisionId :: DecisionId,
    label :: CaseLabel,
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
