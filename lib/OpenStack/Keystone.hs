{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module OpenStack.Keystone where

import           Control.Applicative
import           Control.Lens.Operators     ((^.))
import           Control.Lens.TH
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.HashMap.Strict        as H
import           Data.Monoid
import           Data.Proxy
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Time.Clock
import           Data.Time.Format
import           Servant.API
import           Servant.Client
import           System.Locale

data TokenRequest = TokenRequest
    { _requestTenantName :: Maybe Text
    , _requestUsername   :: Text
    , _requestPassword   :: Text
    } deriving (Eq, Show)
makeLenses ''TokenRequest

instance ToJSON TokenRequest where
    toJSON x = object [ "auth" .= auth ]
      where
        auth :: Value
        auth = object
            [ "tenantName" .= (x ^. requestTenantName)
            , "passwordCredentials" .= passwordCredentials ]
        passwordCredentials :: Value
        passwordCredentials = object
            [ "username" .= (x ^. requestUsername)
            , "password" .= (x ^. requestPassword) ]

newtype ISO8601Time =
    ISO8601Time { unISO8601Time :: UTCTime } deriving (Eq, Show)

instance FromJSON ISO8601Time where
    parseJSON (String x) =
        let x' = T.unpack x
            res = parseTime defaultTimeLocale "%FT%T%Q%Z" x'
              <|> parseTime defaultTimeLocale "%F" x'
        in case res of
            Nothing -> mempty
            Just t -> pure . ISO8601Time $ t
    parseJSON _ = mempty

data Tenant = Tenant
    { _tenantId          :: Text
    , _tenantName        :: Text
    , _tenantEnabled     :: Bool
    , _tenantDescription :: Text
    } deriving (Eq, Show)
makeLenses ''Tenant

instance FromJSON Tenant where
    parseJSON (Object o) = Tenant
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "enabled"
        <*> o .: "description"
    parseJSON _ = mempty

newtype TokenId = TokenId
    { unTokenId :: ByteString
    } deriving (Eq, Show)
makeWrapped ''TokenId

instance FromJSON TokenId where
    parseJSON x = TokenId . T.encodeUtf8 <$> parseJSON x

instance ToJSON TokenId where
    toJSON = toJSON . T.decodeUtf8 . unTokenId

data Token = Token
    { _tokenIssuedAt :: UTCTime
    , _tokenExpires  :: UTCTime
    , _tokenId       :: TokenId
    , _tokenTenant   :: Maybe Tenant
    } deriving (Eq, Show)
makeLenses ''Token

instance FromJSON Token where
    parseJSON (Object o) = Token
        <$> (unISO8601Time <$> o .: "issued_at")
        <*> (unISO8601Time <$> o .: "expires")
        <*> o .: "id"
        <*> o .:? "tenant"
    parseJSON _ = mempty

data Endpoint = Endpoint
    { _endpointAdminUrl    :: Text
    , _endpointInternalUrl :: Text
    , _endpointPublicUrl   :: Text
    , _endpointRegion      :: Text
    , _endpointId          :: Text
    } deriving (Eq, Show)

instance FromJSON Endpoint where
    parseJSON (Object o) = Endpoint
        <$> o .: "adminURL"
        <*> o .: "internalURL"
        <*> o .: "publicURL"
        <*> o .: "region"
        <*> o .: "id"
    parseJSON _ = mempty

data Service = Service
    { _serviceEndpoints :: [Endpoint]
    , _serviceType      :: Text
    , _serviceName      :: Text
    } deriving (Eq, Show)
makeLenses ''Service

instance FromJSON Service where
    parseJSON (Object o) = Service
        <$> o .: "endpoints"
        <*> o .: "type"
        <*> o .: "name"
    parseJSON _ = mempty

data Role = Role
    { _roleName :: Text
    } deriving (Eq, Show)

instance FromJSON Role where
    parseJSON (Object o) = Role
        <$> o .: "name"
    parseJSON _ = mempty

data User = User
    { _userUserName :: Text
    , _userId       :: Text
    , _userRoles    :: [Role]
    , _userName     :: Text
    } deriving (Eq, Show)

instance FromJSON User where
    parseJSON (Object o) = User
        <$> o .: "username"
        <*> o .: "id"
        <*> o .: "roles"
        <*> o .: "name"
    parseJSON _ = mempty

data TokenResponse = TokenResponse
    { _responseToken          :: Token
    , _responseServiceCatalog :: [Service]
    , _responseUser           :: User
    } deriving (Eq, Show)

instance FromJSON TokenResponse where
    parseJSON (Object o) =
        case H.lookup "access" o of
            (Just (Object o')) -> TokenResponse
                <$> o' .: "token"
                <*> o' .: "serviceCatalog"
                <*> o' .: "user"
            _ -> mempty
    parseJSON _ = mempty

type KeystoneApi =
         "v2.0" :> "tokens" :> ReqBody '[JSON] TokenRequest :> Post '[JSON] TokenResponse

keystoneApi :: Proxy KeystoneApi
keystoneApi = Proxy

requestToken :: TokenRequest -> BaseUrl -> EitherT ServantError IO TokenResponse
requestToken = client keystoneApi
