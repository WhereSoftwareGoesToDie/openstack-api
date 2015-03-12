{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module OpenStack.Nova where

import           Control.Applicative
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import           Data.Text                  (Text)
import           Servant.API
import           Servant.Client

import           OpenStack.Common

data Server = Server
    { _serverId    :: ServerId
    , _serverLinks :: [Link]
    , _serverName  :: Text
    } deriving (Eq, Show)

instance FromJSON Server where
    parseJSON (Object o) = Server
        <$> o .: "id"
        <*> o .: "links"
        <*> o .: "name"
    parseJSON _ = mempty

newtype ListServersResponse = ListServersResponse
    { unListServersResponse :: [Server]
    } deriving (Eq, Show)

instance FromJSON ListServersResponse where
    parseJSON (Object o) = ListServersResponse <$> o .: "servers"
    parseJSON _ = mempty

type NovaApi =
         "v2" :> Capture "tenantid" TenantId :> "servers" :> Header "X-Auth-Token" TokenId :> Get '[JSON] ListServersResponse

novaApi :: Proxy NovaApi
novaApi = Proxy

listServers :: TenantId -> Maybe TokenId -> BaseUrl -> EitherT ServantError IO ListServersResponse
listServers = client novaApi
