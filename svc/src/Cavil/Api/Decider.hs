{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Api.Decider where

import Cavil.Api.Decider.Var
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
    _deciderRecordDecision :: route :- Capture "deciderId" DeciderId :> Capture "decisionId" DecisionId :> ReqBody '[JSON] RecordDecisionRequest :> Put '[JSON] RecordDecisionResponse,
    _deciderDecisionSummarise :: route :- Capture "deciderId" DeciderId :> Capture "decisionId" DecisionId :> Get '[JSON] DecisionSummary,
    _deciderDecisionInvalidate :: route :- Capture "deciderId" DeciderId :> Capture "decisionId" DecisionId :> "invalidate" :> ReqBody '[JSON] InvalidateDecisionRequest :> Post '[JSON] NoContent,
    _decidersSummarise :: route :- Get '[JSON] [DeciderSummary]
  }
  deriving stock (Generic)

-- /API.

-- Interface domain types.
newtype DeciderId = DeciderId {unDeciderId :: UUID}
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Ae.ToJSON, Ae.FromJSON, FromHttpApiData)

newtype DecisionId = DecisionId {unDecisionId :: UUID}
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON, Ae.FromJSON, Eq, FromHttpApiData)
  deriving newtype (Ord) -- For use in a Map

newtype DeciderLabel = DeciderLabel {unDeciderLabel :: Text}
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON, Ae.FromJSON, FromHttpApiData)

newtype FieldLabel = FieldLabel {unFieldLabel :: Text}
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Ae.ToJSONKey, Ae.FromJSONKey, FromHttpApiData, ToHttpApiData)

newtype FieldId = FieldId {unFieldId :: UUID}
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON, Ae.FromJSON, Eq, FromHttpApiData)
  deriving newtype (Ord) -- For use in a Map

-- /Interface domain types.

-- Request interfaces.
data CreateDeciderRequest = CreateDeciderRequest
  { label :: DeciderLabel,
    schema :: DeciderSchema
  }
  deriving stock (Generic)
  deriving anyclass (Ae.FromJSON)

type DeciderSchema = Map FieldLabel VariantList

type RecordDecisionRequest = Map FieldLabel (DecisionVar RawVariantSelection)

type RecordDecisionResponse = Map FieldLabel (EvalVar VariantSelection)

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
    schema :: DeciderSchema,
    decisions :: [DecisionSummary]
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON)

data DecisionSummary = DecisionSummary
  { id :: DecisionId,
    creationTime :: T.UTCTime,
    body :: DecisionBody,
    isValid :: Bool,
    invalidationReason :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON)

type DecisionBody = Map FieldLabel (EvalVar VariantSelection)

-- /Response interfaces.
