{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Api.Case where

import Cavil.Api.Common
import Control.Monad.Fail (MonadFail (fail))
import Data.Aeson ((.=))
import qualified Data.Aeson as Ae
import qualified Data.HashMap.Strict as HM
import Data.UUID (UUID)
import Protolude
import Servant
import Servant.API.Generic

-- API.
data CaseRoutes route = CaseRoutes
  { _caseCreate :: route :- Capture "label" CaseLabel :> ReqBody '[JSON] CreateCaseRequest :> Put '[JSON] NoContent,
    _caseSummarise :: route :- Capture "label" CaseLabel :> Get '[JSON] CaseSummary,
    _caseDecide :: route :- Capture "label" CaseLabel :> Capture "decisionToken" DecisionToken :> Put '[JSON] Variant,
    _caseDecisionInvalidate :: route :- Capture "label" CaseLabel :> Capture "decisionToken" DecisionToken :> "invalidate" :> ReqBody '[JSON] InvalidateDecisionRequest :> Post '[JSON] NoContent,
    _casesSummarise :: route :- Get '[JSON] [CaseSummariesItem]
  }
  deriving stock (Generic)

-- /API.

-- Interface domain types.
newtype Variant = Variant {variantInt :: Int}
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

newtype NrVariants = NrVariants {nrVariantsInt :: Int}
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

newtype DecisionToken = DecisionToken {decisionTokenUUID :: UUID}
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON, Ae.FromJSON, Eq, FromHttpApiData)
  deriving newtype (Ord) -- For use in a Map

newtype CaseLabel = CaseLabel {caseLabelText :: Text}
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON, Ae.FromJSON, FromHttpApiData)

-- /Interface domain types.

-- Request interfaces.
data CreateCaseRequest = CreateCaseRequest
  { nrVariants :: NrVariants
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
  { nextDecisionToken :: DecisionToken,
    label :: CaseLabel,
    nrVariants :: NrVariants,
    decisions :: [DecisionSummary]
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON)

data CaseSummariesItem
  = FailedCaseSummariesItem FailedCaseSummary
  | SucceededCaseSummariesItem CaseSummary
  deriving stock (Generic)

data FailedCaseSummary = FailedCaseSummary
  { label :: CaseLabel,
    error :: ClientError
  }
  deriving stock (Generic)

instance Ae.ToJSON FailedCaseSummary where
  toJSON v =
    Ae.object
      [ ("label" :: Text) .= getField @"label" v,
        "error" .= getField @"error" v
      ]

instance Ae.ToJSON CaseSummariesItem where
  toJSON v =
    let (obj, status) = case v of
          SucceededCaseSummariesItem caseSummary ->
            case Ae.toJSON caseSummary of
              Ae.Object obj_ ->
                (obj_, "Success")
              e ->
                panic $ "Unexpected encoding of case summary: " <> show e
          FailedCaseSummariesItem failedSummary ->
            case Ae.toJSON failedSummary of
              Ae.Object obj_ ->
                (obj_, "Failure")
              e ->
                panic $ "Unexpected encoding of failed case summary: " <> show e
     in Ae.Object $ HM.insert "fetchStatus" (Ae.String status) obj

data DecisionSummary = DecisionSummary
  { token :: DecisionToken,
    decisionTimeUTC :: Text,
    variant :: Variant,
    isValid :: Bool,
    invalidationReason :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON)

-- /Response interfaces.
