module Cavil.Api where

import Cavil.Api.Ledger
import Data.Aeson qualified as Ae
import Protolude
import Servant
import Servant.API.Generic

type User = ()

type AuthRealm = "Cavil"

type AuthPrefix = BasicAuth AuthRealm User

data SiteRoutes route = SiteRoutes
  { _ledger :: route :- AuthPrefix :> "ledger" :> ToServant LedgerRoutes AsApi,
    _version :: route :- "version" :> Get '[JSON] Version
  }
  deriving stock (Generic)

newtype Version = Version {unVersion :: Text}
  deriving newtype (Ae.ToJSON)

siteApi :: Proxy (ToServantApi SiteRoutes)
siteApi = genericApi (Proxy :: Proxy SiteRoutes)
