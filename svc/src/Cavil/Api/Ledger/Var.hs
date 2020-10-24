module Cavil.Api.Ledger.Var
  ( Variant (..),
    VariantSelection (..),
    VariantList (..),
    selectVariantByIdx,
    selectVariantByLabel,
    variantSelectionVariant,
    mkVariantList,
    mkSimpleVariantList,
  )
where

import Control.Monad.Fail (MonadFail (fail))
import Data.Aeson qualified as Ae
import Data.Containers.ListUtils (nubOrd)
import Protolude
import Servant.API (FromHttpApiData)

newtype Variant = Variant {unVariant :: Text}
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Ae.ToJSON, Ae.FromJSON, FromHttpApiData)

newtype VariantSelection = UnsafeVariantSelection {unVariantSelection :: Variant}
  deriving stock (Generic, Show)
  deriving newtype (Ae.ToJSON, Eq)
  deriving newtype (Ord) -- For use in a Map

variantSelectionVariant :: VariantSelection -> Variant
variantSelectionVariant = unVariantSelection

selectVariantByIdx :: VariantList -> Int -> Maybe VariantSelection
selectVariantByIdx vl n = do
  v <- unVariantList vl `atMay` n
  pure $ UnsafeVariantSelection v

selectVariantByLabel :: VariantList -> Variant -> Maybe VariantSelection
selectVariantByLabel vl v
  | v `elem` unVariantList vl =
    Just (UnsafeVariantSelection v)
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
mkSimpleVariantList nrVars =
  mkVariantList $ Variant . show <$> (take nrVars [0 ..] :: [Int])
