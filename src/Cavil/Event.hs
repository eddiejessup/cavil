{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Cavil.Event where

import Cavil.Api
import Cavil.Hashing
import qualified Data.Aeson as Ae
import Data.Generics.Product.Typed
import Data.Generics.Sum (AsType, injectTyped)
import qualified Data.List as List
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
import qualified Data.Map.Strict as Map

data DecisionAggregate = DecisionAggregate
  { decisionTime :: UTCTime,
    variant :: Variant,
    status :: DecisionValidity
  }
  deriving stock Generic

data DecisionValidity
  = DecisionIsValid
  | DecisionIsNotValid Text

initialDecisionAggregate :: DecisionMadeEvent -> DecisionAggregate
initialDecisionAggregate ev =
  DecisionAggregate
    { decisionTime = getTyped @UTCTime ev,
      variant = getTyped @Variant ev,
      status = DecisionIsValid
    }

initialCaseAggregate :: CaseCreatedEvent -> CaseAggregate
initialCaseAggregate ev =
  CaseAggregate
    { caseLabel = ev ^. typed @CaseLabel,
      nrVariants = ev ^. typed @NrVariants,
      decisions = Map.empty
    }

type DecisionsMap = Map DecisionToken DecisionAggregate

-- | Do not filter to only valid decisions, or the token chain will lose
-- coherence.
lastDecisionTime :: CaseAggregate -> Maybe UTCTime
lastDecisionTime agg =
  maximumMay $ getTyped @UTCTime <$> Map.elems (getTyped @DecisionsMap agg)

computeNextDecisionToken :: CaseLabel -> Maybe UTCTime -> DecisionToken
computeNextDecisionToken caseLabel mayLastDecisionTime =
  let caseLabelTxt = caseLabel ^. typed @Text
      mayLastDecisionTimeTxt = maybe initialDecisionSalt (Tx.pack . T.iso8601Show) mayLastDecisionTime
   in DecisionToken $ uuidFromArbitraryText (caseLabelTxt <> mayLastDecisionTimeTxt)

nextDecisionTokenFromAgg :: CaseAggregate -> DecisionToken
nextDecisionTokenFromAgg agg =
  computeNextDecisionToken (getTyped @CaseLabel agg) (lastDecisionTime agg)

foldAllEventsIntoAggregate :: (MonadError e m, AsType AggregateError e) => AggregateState -> [CaseEvent] -> m (Maybe CaseAggregate)
foldAllEventsIntoAggregate aggState = foldIncrementalEventsIntoAggregate aggState Nothing

foldIncrementalEventsIntoAggregate :: (MonadError e m, AsType AggregateError e) => AggregateState -> Maybe CaseAggregate -> [CaseEvent] -> m (Maybe CaseAggregate)
foldIncrementalEventsIntoAggregate aggState = foldM (foldEventIntoAggregate aggState)

foldEventIntoAggregate :: (MonadError e m, AsType AggregateError e) => AggregateState -> Maybe CaseAggregate -> CaseEvent -> m (Maybe CaseAggregate)
foldEventIntoAggregate aggState mayAgg = \case
  CaseCreated ccEvt -> case mayAgg of
    Nothing ->
      pure $ Just $ initialCaseAggregate ccEvt
    Just _ ->
      throwError $ aggError CaseAlreadyExists
  DecisionMade ev -> do
    agg <- note (aggError NoSuchCase) mayAgg
    if nextDecisionTokenFromAgg agg == ev ^. typed @DecisionToken
      then
        pure $ Just $ agg & typed @DecisionsMap %~ Map.insert (getTyped @DecisionToken ev) (initialDecisionAggregate ev)
      else
        throwError $ aggError IncoherentDecisionToken
  DecisionInvalidated ev -> do
    let tok = getTyped @DecisionToken ev
    let reason = getField @"reason" ev
    agg <- note (aggError NoSuchCase) mayAgg
    decisionAgg <- note (aggError NoSuchDecision) (Map.lookup tok (getTyped @DecisionsMap agg))
    case getTyped @DecisionValidity decisionAgg of
      DecisionIsNotValid _ ->
        throwError $ aggError DecisionAlreadyInvalidated
      DecisionIsValid ->
        pure $ Just $ agg & typed @DecisionsMap % ix tok % typed @DecisionValidity .~ DecisionIsNotValid reason
  where
    aggError e =
      injectTyped $ AggregateError aggState e

getAggregate ::
  (MonadIO m, MonadError e m, AsType AggregateError e, MonadReader r m, HasType PG.Connection r) =>
  AggregateState ->
  m (Maybe CaseAggregate, AggregateValidatedToken)
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
              event_type = 'case'
              and aggregate_id = ?
            ORDER BY
              sequence_nr ASC
          |]
        (PG.Only aggId)
  agg <- foldAllEventsIntoAggregate aggState (PG.fromOnly <$> evts)
  pure (agg, AggregateValidatedToken aggId)

getAllEvents ::
  (MonadIO m, MonadReader r m, HasType PG.Connection r) =>
  m [CaseEvent]
getAllEvents = do
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
            event_type = 'case'
          ORDER BY
            sequence_nr ASC
        |]
        ()
  pure $ PG.fromOnly <$> evts

insertEventsValidated ::
  (MonadIO m, MonadError e m, AsType AggregateError e, AsType WriteError e, MonadReader r m, HasType PG.Connection r) =>
  AggregateValidatedToken ->
  Maybe CaseAggregate ->
  [CaseEvent] ->
  m (Maybe CaseAggregate)
insertEventsValidated valAggId curAgg evts = do
  -- Check inserting event should go okay.
  newAgg <- foldIncrementalEventsIntoAggregate (AggregateDuringRequest valAggId) curAgg evts
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
        [sql| INSERT INTO event (event_type, aggregate_id, data) VALUES (?, ?, ?)|]
        ( List.zip3
            (repeat @Text "case")
            (repeat (validatedAggId valAggId))
            evts
        )

  when (nrRowsAffected /= length evts)
    $ throwError
    $ injectTyped
    $ InsertedUnexpectedNrRows (fromIntegral nrRowsAffected)

data CaseEvent
  = CaseCreated CaseCreatedEvent
  | DecisionMade DecisionMadeEvent
  | DecisionInvalidated DecisionInvalidatedEvent
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

data DecisionInvalidatedEvent = DecisionInvalidatedEvent
  { decisionToken :: DecisionToken,
    reason :: Text
  }
  deriving stock (Generic)
  deriving anyclass (Ae.ToJSON, Ae.FromJSON)

data CaseAggregate = CaseAggregate
  { caseLabel :: CaseLabel,
    nrVariants :: NrVariants,
    decisions :: DecisionsMap
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
  | NoSuchDecision
  | IncoherentDecisionToken
  | DecisionAlreadyInvalidated
  deriving stock (Generic, Show)

data WriteError
  = InsertedUnexpectedNrRows Int
  deriving stock (Generic, Show)
