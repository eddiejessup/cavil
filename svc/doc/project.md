# Projection

I'm just keeping this here to not clutter up the code, but if it ever becomes a problem to traverse all events to project out information, we can do something like this. Except, let's not project tables and instead project in-memory and serve the data over HTTP. Because managing the state of the tables will be a faff.

projectMain :: IO ()
projectMain = do
  Options {pgConnectInfo, listenPort} <- execParser optsInfo
  pgConn <- PG.connect pgConnectInfo
  let maxSequenceNrSeen = Nothing

updateProjection :: PG.Connection -> Maybe Int64 -> IO (Maybe Int64)
updateProjection pgConn maxSequenceNrAlreadySeen = do
  rows <- liftIO $ PG.query @_ @(Int64, AggregateId, CaseEvent) pgConn
    [sql|
        SELECT
          sequence_nr,
          aggregate_id,
          data
        FROM
          event
        WHERE
          sequence_nr > ?
        ORDER BY
          sequence_nr ASC
      |]
    (PG.Only (fromMaybe 0 maxSequenceNrAlreadySeen))

  PG.withTransaction pgConn $ forM_ rows \(_, aggId, evt) -> do
    let (query, params) = projectCaseLabel aggId evt
    PG.executeMany pgConn query params

  let
    maxSequenceNrNewlySeen maximumMay (rows <&> (\(sId, _, _) -> sId))
    newMaxSequenceNr = case (maxSequenceNrAlreadySeen, maxSequenceNrNewlySeen) of
      (Nothing, Nothing) -> Nothing
      (Just x, Nothing) -> Just x
      (Just x, Just y) -> Just y
      (Nothing, Just y) -> Just y

  pure newMaxSequenceNr
