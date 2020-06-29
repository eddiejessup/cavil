{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Cavil.Serve
import qualified Database.PostgreSQL.Simple as PG
import Network.Wai.Handler.Warp (run)
import Options.Applicative
import Protolude hiding (option)

pgOpts :: Parser PG.ConnectInfo
pgOpts =
  PG.ConnectInfo
    <$> strOption
      ( long "pg-host"
          <> metavar "PG_HOST"
      )
    <*> option
      auto
      ( long "pg-port"
          <> metavar "PG_PORT"
      )
    <*> strOption
      ( long "pg-user"
          <> metavar "PG_USER"
      )
    <*> strOption
      ( long "pg-password"
          <> metavar "PG_PASSWORD"
      )
    <*> strOption
      ( long "pg-db"
          <> metavar "PG_DB"
      )

data Options = Options
  { pgConnectInfo :: PG.ConnectInfo,
    listenPort :: Word16
  }

optsParser :: Parser Options
optsParser =
  Options
    <$> pgOpts
    <*> option
      auto
      ( long "listen-port"
          <> metavar "LISTEN_PORT"
      )

optsInfo :: ParserInfo Options
optsInfo =
  info
    (optsParser <**> helper)
    ( fullDesc
        <> progDesc "Serve Cavil API"
        <> header "cavil - tracked, randomised decisions"
    )

main :: IO ()
main = do
  Options { pgConnectInfo, listenPort } <- execParser optsInfo
  pgConn <- PG.connect pgConnectInfo
  let env = AppEnv { pgConn }
  putText $ "Starting at http://localhost:" <> show listenPort
  run (fromIntegral listenPort) (app env)
