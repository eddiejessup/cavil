{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Event where

import Cavil.Api
import Cavil.Hashing
import qualified Data.Aeson as Ae
import Data.Generics.Product.Typed
import Data.Generics.Sum (AsType, injectTyped)
import qualified Data.Text as Tx
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Format.ISO8601 as T
import Data.UUID (UUID)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple.ToField as PG
import Optics
import Protolude hiding ((%), to)

foldEvents :: (MonadError e m, AsType AggregateError e) => AggregateState -> [CaseEvent] -> m (Maybe Aggregate)
foldEvents aggState = foldEventsIntoAggregate aggState Nothing

foldEventsIntoAggregate :: (MonadError e m, AsType AggregateError e) => AggregateState -> Maybe Aggregate -> [CaseEvent] -> m (Maybe Aggregate)
foldEventsIntoAggregate aggState = foldM (foldEvent aggState)

lastDecisionTime :: Aggregate -> Maybe UTCTime
lastDecisionTime agg =
  (\(_, t, _) -> t) <$> lastMay (getTyped @[(DecisionToken, UTCTime, Variant)] agg)

nextDecisionToken :: CaseLabel -> Maybe UTCTime -> DecisionToken
nextDecisionToken caseLabel mayLastDecisionTime =
  let caseLabelTxt = caseLabel ^. typed @Text
      mayLastDecisionTimeTxt = maybe mempty (Tx.pack . T.iso8601Show) mayLastDecisionTime
   in DecisionToken $ uuidFromArbitraryText (caseLabelTxt <> mayLastDecisionTimeTxt)

nextDecisionTokenFromAgg :: Aggregate -> DecisionToken
nextDecisionTokenFromAgg agg =
  nextDecisionToken (getTyped @CaseLabel agg) (lastDecisionTime agg)

foldEvent :: (MonadError e m, AsType AggregateError e) => AggregateState -> Maybe Aggregate -> CaseEvent -> m (Maybe Aggregate)
foldEvent aggState mayAgg = \case
  CaseCreated ccEvt -> case mayAgg of
    Nothing ->
      pure $ Just $ Aggregate (ccEvt ^. typed @CaseLabel) (ccEvt ^. typed @NrVariants) []
    Just _ ->
      throwAggError CaseAlreadyExists
  DecisionMade ev -> case mayAgg of
    Nothing ->
      throwAggError NoSuchCase
    Just agg
      | nextDecisionTokenFromAgg agg == ev ^. typed @DecisionToken ->
        pure $ Just $
          agg
            & typed @[(DecisionToken, UTCTime, Variant)] %~ (<> [(getTyped @DecisionToken ev, getTyped @UTCTime ev, getTyped @Variant ev)])
      | otherwise ->
        throwAggError IncoherentDecisionToken
  where
    throwAggError e =
      throwError $ injectTyped $ AggregateError aggState e

getAggregate ::
  (MonadIO m, MonadError e m, AsType AggregateError e, MonadReader r m, HasType PG.Connection r) =>
  AggregateState ->
  m (Maybe Aggregate, AggregateValidatedToken)
getAggregate aggState = do
  let aggId = aggIdFromState aggState
  pgConn <- gview (typed @PG.Connection)
  evts <-
    liftIO $
      PG.query @_ @(PG.Only CaseEvent)
        pgConn
        [sql|
            SELECT
              data
            FROM
              event
            WHERE
              aggregate_id = ?
            ORDER BY
              sequence_nr ASC
          |]
        (PG.Only aggId)
  agg <- foldEvents aggState (PG.fromOnly <$> evts)
  pure (agg, AggregateValidatedToken aggId)

insertEventsValidated ::
  (MonadIO m, MonadError e m, AsType AggregateError e, AsType WriteError e, MonadReader r m, HasType PG.Connection r) =>
  AggregateValidatedToken ->
  Maybe Aggregate ->
  [CaseEvent] ->
  m (Maybe Aggregate)
insertEventsValidated valAggId curAgg evts = do
  -- Check inserting event should go okay.
  newAgg <- foldEventsIntoAggregate (AggregateDuringRequest valAggId) curAgg evts
  insertEvents valAggId evts
  pure newAgg

insertEvents ::
  (MonadIO m, MonadError e m, AsType WriteError e, MonadReader r m, HasType PG.Connection r) =>
  AggregateValidatedToken ->
  [CaseEvent] ->
  m ()
insertEvents valAggId evts = do
  pgConn <- gview (typed @PG.Connection)
  nrRowsAffected <-
    fmap (fromIntegral @Int64 @Int) $ liftIO $
      PG.executeMany
        pgConn
        [sql| INSERT INTO event (aggregate_id, data) VALUES (?, ?)|]
        (zip (repeat (validatedAggId valAggId)) evts)

  when (nrRowsAffected /= length evts)
    $ throwError
    $ injectTyped
    $ InsertedUnexpectedNrRows (fromIntegral nrRowsAffected)

data CaseEvent
  = CaseCreated CaseCreatedEvent
  | DecisionMade DecisionMadeEvent
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON, Ae.FromJSON)

instance PG.FromField CaseEvent where
  fromField = PG.fromJSONField

instance PG.ToField CaseEvent where
  toField = PG.toJSONField

data CaseCreatedEvent = CaseCreatedEvent
  { caseLabel :: CaseLabel,
    nrVariants :: NrVariants
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON, Ae.FromJSON)

data DecisionMadeEvent = DecisionMadeEvent
  { decisionToken :: DecisionToken,
    variant :: Variant,
    decisionTime :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON, Ae.FromJSON)

data Aggregate = Aggregate
  { caseLabel :: CaseLabel,
    nrVariants :: NrVariants,
    decisions :: [(DecisionToken, UTCTime, Variant)]
  }
  deriving stock (Generic)

data AggregateState
  = AggregateBeforeRequest AggregateID
  | AggregateDuringRequest AggregateValidatedToken
  deriving stock (Generic)

newtype AggregateID = AggregateID {aggIdUUID :: UUID}
  deriving stock (Generic, Show)
  deriving newtype (PG.ToField, PG.FromField)

aggIdFromState :: AggregateState -> AggregateID
aggIdFromState = \case
  AggregateBeforeRequest aggId -> aggId
  AggregateDuringRequest valAggId -> validatedAggId valAggId

newtype AggregateValidatedToken = AggregateValidatedToken {validatedAggId :: AggregateID}

data AggregateError
  = AggregateError AggregateState AggregateErrorDetail
  deriving stock (Generic)

data AggregateErrorDetail
  = CaseAlreadyExists
  | NoSuchCase
  | IncoherentDecisionToken
  deriving stock (Generic, Show)

data WriteError
  = InsertedUnexpectedNrRows Int
  deriving stock (Generic, Show)
