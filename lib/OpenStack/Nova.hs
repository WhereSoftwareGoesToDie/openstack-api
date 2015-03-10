{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module OpenStack.Nova where

import           Control.Applicative
import           Control.Lens.TH
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.ByteString            (ByteString)
import           Data.Monoid
import           Data.Proxy
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as T
import           Servant.API
import           Servant.Client

import           OpenStack.Keystone

newtype ServerId = ServerId
    { unServerId :: ByteString
    } deriving (Eq, Show)
makeWrapped ''ServerId

instance FromJSON ServerId where
    parseJSON x = ServerId . T.encodeUtf8 <$> parseJSON x

instance ToJSON ServerId where
    toJSON = toJSON . T.decodeUtf8 . unServerId

instance ToText ServerId where
    toText = T.decodeUtf8 . unServerId

data Link = Link
    { _linkHref :: Text
    , _linkRel  :: Text
    } deriving (Eq, Show)

instance FromJSON Link where
    parseJSON (Object o) = Link
        <$> o .: "href"
        <*> o .: "rel"
    parseJSON _ = mempty

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
