module Cavil.Api.Decider.Var
  ( EvalVar (..),
    DecisionVar (..),
    Variant (..),
    RawVariantSelection (..),
    VariantSelection (..),
    VariantList (..),
    selectVariantByIdx,
    selectVariantByLabel,
    variantSelectionVariant,
    mkVariantList,
    mkSimpleVariantList,
    collectMaybeEvalVar,
  )
where

import Control.Monad.Fail (MonadFail (fail))
import Data.Aeson ((.:))
import Data.Aeson qualified as Ae
import Data.Containers.ListUtils (nubOrd)
import Data.Time.Clock qualified as T
import Protolude
import Servant.API (FromHttpApiData)

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

newtype Variant = Variant {unVariant :: Text}
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
  | length (nubOrd vs) /= length vs = Nothing
  | otherwise = Just $ VariantList vs

mkSimpleVariantList :: Int -> Maybe VariantList
mkSimpleVariantList nrVars
  = mkVariantList $ Variant . show <$> (take nrVars [0..] :: [Int])

newtype RawVariantSelection = RawVariantSelection {unRawVariantSelection :: Variant}
  deriving stock (Generic, Show)
  deriving newtype (Ae.FromJSON, Eq, FromHttpApiData)
  deriving newtype (Ord) -- For use in a Map
