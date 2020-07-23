{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Options where

import qualified Data.List as List
import Data.String (String)
import qualified Data.Text as Tx
import qualified Database.PostgreSQL.Simple as PG
import Options.Applicative
import Protolude hiding (option)
import Safe (readMay)

type EnvVars = [(EnvVarKey, EnvVarVal)]

type EnvVarKey = String

type EnvVarVal = String

class FromEnvVarVal a where
  fromString :: EnvVarVal -> Either String a

instance FromEnvVarVal Text where
  fromString = Right . Tx.pack

instance FromEnvVarVal String where
  fromString = Right

instance FromEnvVarVal Word16 where
  fromString s = case readMay s of
    Nothing -> Left $ "Could not read " <> show s <> " as Word16"
    Just v -> pure v

-- | idm :: Monoid m => m, the trivial option modifier.
-- look up a key 'k' from the environment, and try to parse it.
envVarDefault :: (HasValue f, FromEnvVarVal a) => EnvVarKey -> EnvVars -> Mod f a
envVarDefault varName envVars = maybe idm value (List.lookup varName envVars >>= parse)
  where
    parse envValStr =
      case fromString envValStr of
        Left _ -> Nothing
        Right v -> Just v

pgOptsParser :: EnvVars -> Parser PG.ConnectInfo
pgOptsParser envVars =
  PG.ConnectInfo
    <$> strOption
      ( long "pg-host"
          <> metavar "PG_HOST"
          <> envVarDefault "PG_HOST" envVars
          <> showDefault
      )
    <*> option
      auto
      ( long "pg-port"
          <> metavar "PG_PORT"
          <> envVarDefault "PG_PORT" envVars
          <> showDefault
      )
    <*> strOption
      ( long "pg-user"
          <> metavar "PG_USER"
          <> envVarDefault "PG_USER" envVars
          <> showDefault
      )
    <*> strOption
      ( long "pg-password"
          <> metavar "PG_PASSWORD"
          <> envVarDefault "PG_PASSWORD" envVars
          <> showDefault
      )
    <*> strOption
      ( long "pg-db"
          <> metavar "PG_DB"
          <> envVarDefault "PG_DB" envVars
          <> showDefault
      )

data AuthOpts = AuthOpts
  { clientUsername :: Text,
    clientPassword :: Text,
    allowedCorsOrigin :: Maybe Text
  }

authOptsParser :: EnvVars -> Parser AuthOpts
authOptsParser envVars =
  AuthOpts
    <$> strOption
      ( long "client-username"
          <> metavar "CLIENT_USERNAME"
          <> envVarDefault "CLIENT_USERNAME" envVars
          <> showDefault
      )
    <*> strOption
      ( long "client-password"
          <> metavar "CLIENT_PASSWORD"
          <> envVarDefault "CLIENT_PASSWORD" envVars
          <> showDefault
      )
    <*> optional
      ( strOption
          ( long "allowed-cors-origin"
              <> metavar "ALLOWED_CORS_ORIGIN"
              <> envVarDefault "ALLOWED_CORS_ORIGIN" envVars
              <> showDefault
          )
      )

data Options = Options
  { pgConnectInfo :: PG.ConnectInfo,
    authOpts :: AuthOpts,
    listenPort :: Word16
  }

optsParser :: EnvVars -> Parser Options
optsParser envVars =
  Options
    <$> pgOptsParser envVars
    <*> authOptsParser envVars
    <*> listenPortParser
  where
    listenPortParser =
      option
        auto
        ( long "listen-port"
            <> metavar "LISTEN_PORT"
            <> envVarDefault "LISTEN_PORT" envVars
            <> showDefault
        )

optsInfo :: EnvVars -> ParserInfo Options
optsInfo envVars =
  info
    (optsParser envVars <**> helper)
    ( fullDesc
        <> progDesc "Serve Cavil API"
        <> header "cavil - tracked, randomised decisions"
    )
