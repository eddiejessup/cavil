module Main where

import Cavil.Serve
import Cavil.Serve.Common
import Data.Default qualified as Default
import Database.PostgreSQL.Simple qualified as PG
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors qualified as Cors
import Network.Wai.Middleware.RequestLogger qualified as ReqLog
import Network.Wai.Middleware.RequestLogger.JSON qualified as ReqLog
import Options
import Options.Applicative
import Protolude hiding (option)
import System.Environment (getEnvironment)

requestLoggerSettings :: ReqLog.RequestLoggerSettings
requestLoggerSettings =
  Default.def
    { ReqLog.outputFormat = ReqLog.CustomOutputFormatWithDetails ReqLog.formatAsJSON
    }

corsResourcePolicy :: Maybe Text -> Cors.CorsResourcePolicy
corsResourcePolicy mayAllowedOrigin =
  Cors.CorsResourcePolicy
    { Cors.corsOrigins =
        mayAllowedOrigin <&> \orig ->
          ([encodeUtf8 orig], credentialsNeeded),
      Cors.corsMethods = Cors.simpleMethods ++ ["PUT"],
      Cors.corsRequestHeaders = ["Content-Type", "Authorization"],
      Cors.corsExposedHeaders = Nothing,
      Cors.corsMaxAge = Nothing,
      Cors.corsVaryOrigin = False,
      Cors.corsRequireOrigin = False,
      Cors.corsIgnoreFailures = False
    }
  where
    credentialsNeeded = True

main :: IO ()
main = do
  envVars <- getEnvironment
  Options {pgConnectInfo, authOpts, listenPort} <- execParser (optsInfo envVars)
  pgConn <- PG.connect pgConnectInfo
  let appEnv =
        AppEnv
          { pgConn,
            clientUsername = getField @"clientUsername" authOpts,
            clientPassword = getField @"clientPassword" authOpts
          }

  logMiddleware <- ReqLog.mkRequestLogger requestLoggerSettings

  let corsMiddleware = Cors.cors $ const $ Just $ corsResourcePolicy $ allowedCorsOrigin authOpts

  putText $ "Starting at http://localhost:" <> show listenPort
  let webApp = logMiddleware $ corsMiddleware $ mkWebApplication appEnv

  run (fromIntegral listenPort) webApp
