{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Cavil.Serve
import qualified Data.Default as Default
import qualified Database.PostgreSQL.Simple as PG
import Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Middleware.RequestLogger as ReqLog
import qualified Network.Wai.Middleware.RequestLogger.JSON as ReqLog
import Options.Applicative
import Protolude hiding (option)

pgOptsParser :: Parser PG.ConnectInfo
pgOptsParser =
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

data AuthOpts = AuthOpts
  { clientUsername :: Text,
    clientPassword :: Text
  }

authOptsParser :: Parser AuthOpts
authOptsParser =
  AuthOpts
    <$> strOption
      ( long "client-username"
          <> metavar "CLIENT_USERNAME"
      )
    <*> strOption
      ( long "client-password"
          <> metavar "CLIENT_PASSWORD"
      )

data Options = Options
  { pgConnectInfo :: PG.ConnectInfo,
    authOpts :: AuthOpts,
    listenPort :: Word16
  }

optsParser :: Parser Options
optsParser =
  Options
    <$> pgOptsParser
    <*> authOptsParser
    <*> listenPortParser
  where
    listenPortParser =
      option
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

requestLoggerSettings :: ReqLog.RequestLoggerSettings
requestLoggerSettings =
  Default.def
    { ReqLog.outputFormat = ReqLog.CustomOutputFormatWithDetails ReqLog.formatAsJSON
    }

main :: IO ()
main = do
  Options {pgConnectInfo, authOpts, listenPort} <- execParser optsInfo
  pgConn <- PG.connect pgConnectInfo
  logMiddleware <- ReqLog.mkRequestLogger requestLoggerSettings
  let
    env = AppEnv
      { pgConn,
        clientUsername = getField @"clientUsername" authOpts,
        clientPassword = getField @"clientPassword" authOpts
      }
  putText $ "Starting at http://localhost:" <> show listenPort
  let webApp = logMiddleware $ mkWebApplication env
  run (fromIntegral listenPort) webApp
