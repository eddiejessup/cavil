{-# LANGUAGE DuplicateRecordFields #-}

module Cavil.Api where

import Control.Monad.Fail (MonadFail (fail))
import qualified Data.Aeson as Ae
import Data.UUID (UUID)
import Protolude
import Servant
import Servant.API.Generic

data Routes route = Routes
  { _create :: route :- "case" :> Capture "label" CaseLabel :> ReqBody '[JSON] CreateCaseRequest :> Put '[JSON] NoContent,
    _summarise :: route :- "case" :> Capture "label" CaseLabel :> Get '[JSON] CaseSummary,
    _decide :: route :- "case" :> Capture "label" CaseLabel :> ReqBody '[JSON] DecisionRequest :> Post '[JSON] Variant
  }
  deriving stock (Generic)

newtype Variant = Variant Int
  deriving stock (Generic)
  deriving newtype (Ae.ToJSON)

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

newtype NrVariants = NrVariants Int
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

newtype DecisionToken = DecisionToken UUID
  deriving stock (Generic)
  deriving newtype (Ae.ToJSON, Ae.FromJSON, Eq)

newtype CaseLabel = CaseLabel Text
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON, Ae.FromJSON, FromHttpApiData)

data CreateCaseRequest = CreateCaseRequest
  { nrVariants :: NrVariants
  }
  deriving stock (Generic)
  deriving anyclass (Ae.FromJSON)

data DecisionRequest = DecisionRequest
  { decisionToken :: DecisionToken
  }
  deriving stock (Generic)
  deriving anyclass (Ae.FromJSON)

data CaseSummary = CaseSummary
  { decisionToken :: DecisionToken
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON)

api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)
