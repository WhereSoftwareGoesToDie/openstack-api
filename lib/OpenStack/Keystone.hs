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
import           Data.Monoid
import           Data.Proxy
import           Data.Text                  (Text)
import           Data.Time.Clock
import           Servant.API
import           Servant.Client

data TokenRequest = TokenRequest
    { _tenantName :: Maybe Text
    , _username   :: Text
    , _password   :: Text
    } deriving (Eq, Show)
makeLenses ''TokenRequest

instance ToJSON TokenRequest where
    toJSON x = object [ "auth" .= auth ]
      where
        auth :: Value
        auth = object
            [ "tenantName" .= (x ^. tenantName)
            , "passwordCredentials" .= passwordCredentials ]
        passwordCredentials :: Value
        passwordCredentials = object
            [ "username" .= (x ^. username)
            , "password" .= (x ^. password) ]

data TokenResponse = TokenResponse
    { _token :: Token } deriving (Eq, Show)

instance FromJSON TokenResponse where
    parseJSON (Object o) = TokenResponse
        <$> o .: "token"
    parseJSON _ = mempty

data Token = Token
    { _tokenIssuedAt :: UTCTime
    , _tokenExpires  :: UTCTime
    , _TokenId       :: Text
    } deriving (Eq, Show)
makeLenses ''Token

instance FromJSON Token where
    parseJSON (Object o) = Token
        <$> o .: "issued_at"
        <*> o .: "expires"
        <*> o .: "id"
    parseJSON _ = mempty

type KeystoneApi =
         "v2.0" :> "tokens" :> ReqBody TokenRequest :> Post TokenResponse

keystoneApi :: Proxy KeystoneApi
keystoneApi = Proxy

requestToken :: TokenRequest -> BaseUrl -> EitherT String IO TokenResponse
requestToken = client keystoneApi
