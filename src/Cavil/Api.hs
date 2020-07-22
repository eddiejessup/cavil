{-# LANGUAGE DuplicateRecordFields #-}

module Cavil.Api where

import Cavil.Api.Case
import Protolude
import Servant
import Servant.API.Generic

type User = ()

type AuthRealm = "Cavil cases"

type AuthPrefix = BasicAuth AuthRealm User

data SiteRoutes route = SiteRoutes
  { _case :: route :- AuthPrefix :> "case" :> ToServant CaseRoutes AsApi
  }
  deriving stock (Generic)

siteApi :: Proxy (ToServantApi SiteRoutes)
siteApi = genericApi (Proxy :: Proxy SiteRoutes)
