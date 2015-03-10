{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module OpenStack.Keystone where

import           Control.Applicative
import           Control.Lens.Operators     ((^.))
import           Control.Lens.TH
import           Control.Monad.Trans.Either
import           Data.Aeson
import qualified Data.HashMap.Strict        as H
import           Data.Monoid
import           Data.Proxy
import           Data.Text                  (Text)
import qualified Data.Text                  as T
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

data Token = Token
    { _tokenIssuedAt :: UTCTime
    , _tokenExpires  :: UTCTime
    , _tokenId       :: Text
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

data TokenResponse = TokenResponse
    { _token :: Token } deriving (Eq, Show)

instance FromJSON TokenResponse where
    parseJSON (Object o) =
        case H.lookup "access" o of
            (Just (Object o')) -> TokenResponse <$> o' .: "token"
            _ -> mempty
    parseJSON _ = mempty

type KeystoneApi =
         "v2.0" :> "tokens" :> ReqBody '[JSON] TokenRequest :> Post '[JSON] TokenResponse

keystoneApi :: Proxy KeystoneApi
keystoneApi = Proxy

requestToken :: TokenRequest -> BaseUrl -> EitherT ServantError IO TokenResponse
requestToken = client keystoneApi
