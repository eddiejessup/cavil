{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Cavil.Serve where

import Cavil.Api
import Cavil.Serve.Common
import Cavil.Serve.Ledger
#ifndef __GHCIDE__
import qualified Distribution.PackageDescription.TH as P
#endif
import Protolude hiding (Handler)
import Servant
import Servant.API.Generic
import Servant.Server.Generic

siteRoutes :: SiteRoutes (AsServerT AppM)
siteRoutes =
  SiteRoutes
    { _ledger = toServant . ledgerRoutes,
      _version = pure version
    }

-- When GHCIDE encounters at least some usages of Template Haskell, including
-- this usage, GHCIDE experiences a segmentation fault. This C++ guard causes
-- GHCIDE to read an harmless simulacrum of the real code, instead of the true
-- horror.
version :: Version
#ifdef __GHCIDE__
version = Version "0"
#else
version = Version $(P.packageVariable (P.pkgVersion . P.package))
#endif

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
