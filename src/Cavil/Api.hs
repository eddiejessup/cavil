{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Api where

import Control.Monad.Fail (MonadFail (fail))
import qualified Data.Aeson as Ae
import qualified Data.HashMap.Strict as HM
import Data.UUID (UUID)
import Protolude
import Servant
import Servant.API.Generic

-- API.
type User = ()

type AuthRealm = "Cavil cases"

type AuthPrefix = BasicAuth AuthRealm User

data Routes route = Routes
  { _caseCreate :: route :- AuthPrefix :> "case" :> Capture "label" CaseLabel :> ReqBody '[JSON] CreateCaseRequest :> Put '[JSON] NoContent,
    _caseSummarise :: route :- AuthPrefix :> "case" :> Capture "label" CaseLabel :> Get '[JSON] CaseSummary,
    _caseDecide :: route :- AuthPrefix :> "case" :> Capture "label" CaseLabel :> Capture "decisionToken" DecisionToken :> Put '[JSON] Variant,
    _caseDecisionInvalidate :: route :- AuthPrefix :> "case" :> Capture "label" CaseLabel :> Capture "decisionToken" DecisionToken :> "invalidate" :> ReqBody '[JSON] InvalidateDecisionRequest :> Post '[JSON] NoContent,
    _casesSummarise :: route :- AuthPrefix :> "case" :> Get '[JSON] [MultipackCaseSummary]
  }
  deriving stock (Generic)

api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)

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

data MultipackCaseSummary
  = FailedCaseSummary ClientError
  | SucceededCaseSummary CaseSummary
  deriving stock (Generic)

instance Ae.ToJSON MultipackCaseSummary where
  toJSON v =
    let (obj, status) = case v of
          SucceededCaseSummary caseSummary ->
            case Ae.toJSON caseSummary of
              Ae.Object obj_ ->
                (obj_, "Success")
              e ->
                panic $ "Unexpected encoding of case summary: " <> show e
          FailedCaseSummary err ->
            (clientErrorAsObject err, "Failure")
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

-- .. Errors.
data ClientError = ClientError ClientErrorReason (HM.HashMap Text Ae.Value)
  deriving stock (Generic)

clientErrorAsObject :: ClientError -> HM.HashMap Text Ae.Value
clientErrorAsObject (ClientError reason vals) =
  HM.insert "reason" (renderClientReason reason) vals

data ClientErrorReason
  = OurFault
  | BadRequest
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON)

renderClientReason :: IsString s => ClientErrorReason -> s
renderClientReason = \case
  OurFault -> "ourFault"
  BadRequest -> "badRequest"

-- .. /Errors.
-- /Response interfaces.
