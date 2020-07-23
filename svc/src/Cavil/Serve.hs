{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Cavil.Serve where

import Cavil.Api
import Cavil.Serve.Case
import Cavil.Serve.Ledger
import Cavil.Serve.Common
import Protolude hiding (Handler)
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import GitHash

siteRoutes :: SiteRoutes (AsServerT AppM)
siteRoutes =
  SiteRoutes
    { _case = toServant . caseRoutes,
      _ledger = toServant . ledgerRoutes,
      _version = pure version
    }

version :: VersionSummary
version =
  let gi = $$tGitInfoCwd
  in VersionSummary
    { gitHash = toS $ giHash gi,
      gitCommitDate = toS $ giCommitDate gi
    }

-- Natural transformation from our custom handler monad to the servant monad.
appToHandler :: AppEnv -> AppM a -> Handler a
appToHandler env m = runReaderT m env

mkWebApplication :: AppEnv -> Application
mkWebApplication env = genericServeTWithContext (appToHandler env) siteRoutes ctx
  where
    ctx = checkBasicAuth env :. EmptyContext

checkBasicAuth :: AppEnv -> BasicAuthCheck User
checkBasicAuth env = BasicAuthCheck \basicAuthData ->
  let username = decodeUtf8 $ basicAuthUsername basicAuthData
      password = decodeUtf8 $ basicAuthPassword basicAuthData
   in pure $
        if username == clientUsername env
          then
            if password == clientPassword env
              then Authorized ()
              else BadPassword
          else NoSuchUser
