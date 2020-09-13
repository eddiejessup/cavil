{-# LANGUAGE ImportQualifiedPost #-}

module Cavil.Api.Decider.Var
    ( EvalVar(..),
      DecisionVar(..),
      Variant(..),
      RawVariantSelection(..),
      VariantSelection,
      VariantList(..),
      selectVariantByIdx,
      selectVariantByLabel,
      variantSelectionVariant,
      mkVariantList,
      collectMaybeEvalVar,
    )
    where

import Protolude
import Data.Aeson ((.:))
import Data.Aeson qualified as Ae
import Data.Time.Clock qualified as T
import Servant.API (FromHttpApiData)
import Control.Monad.Fail (MonadFail(fail))

data DecisionVar a
  = RandomDecisionVar
  | ManualDecisionVar (EvalVar a)
  deriving stock (Generic)

instance Ae.FromJSON a => Ae.FromJSON (DecisionVar a) where
  parseJSON = \case
    Ae.Null -> pure RandomDecisionVar
    v -> ManualDecisionVar <$> Ae.parseJSON v

data EvalVar a = EvalVar
  { v :: a,
    decisionTime :: T.UTCTime
  }
  deriving stock (Generic, Functor)
  deriving anyclass (Ae.ToJSON)

-- Mimicks API of 'distributive', but not worth adding it as a dependency.
distributeMaybeEvalVar :: EvalVar (Maybe a) -> Maybe (EvalVar a)
distributeMaybeEvalVar EvalVar {v = Nothing} = Nothing
distributeMaybeEvalVar EvalVar {v = Just a, decisionTime} = Just EvalVar {v = a, decisionTime}

collectMaybeEvalVar :: (a -> Maybe b) -> EvalVar a -> Maybe (EvalVar b)
collectMaybeEvalVar f = distributeMaybeEvalVar . fmap f
-- /Mimicks API of 'distributive'

instance Ae.FromJSON a => Ae.FromJSON (EvalVar a) where
  parseJSON =
    Ae.withObject "EvalVar" $ \obj ->
      EvalVar
        <$> obj .: "v"
        <*> obj .: "decisionTime"


newtype Variant = UnsafeVariant {unVariant :: Text}
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Ae.ToJSON, Ae.FromJSON, FromHttpApiData)

newtype VariantSelection = UnsafeVariantSelection {unVariantSelection :: Variant}
  deriving stock (Generic, Show)
  deriving newtype (Ae.FromJSON, Ae.ToJSON, Eq)
  deriving newtype (Ord) -- For use in a Map

variantSelectionVariant :: VariantSelection -> Variant
variantSelectionVariant = unVariantSelection

selectVariantByIdx :: VariantList -> Int -> Maybe VariantSelection
selectVariantByIdx vl n = do
    v <- unVariantList vl `atMay` n
    pure $ UnsafeVariantSelection v

selectVariantByLabel :: VariantList -> RawVariantSelection -> Maybe VariantSelection
selectVariantByLabel vl v
    | unRawVariantSelection v `elem` (unVariantList vl) =
        Just (UnsafeVariantSelection (unRawVariantSelection v))
    | otherwise = Nothing

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

newtype RawVariantSelection = RawVariantSelection { unRawVariantSelection :: Variant }
  deriving stock (Generic, Show)
  deriving newtype (Ae.FromJSON, Eq, FromHttpApiData)
  deriving newtype (Ord) -- For use in a Map
