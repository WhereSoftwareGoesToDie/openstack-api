{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module OpenStack.Keystone where

import           Control.Applicative
import           Control.Lens.Operators     ((^.))
import           Control.Lens.TH
import           Control.Monad.Error
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Coerce
import           Data.Monoid
import           Data.Proxy
import           Data.Text                  (Text)
import qualified Data.Vector                as V
import           Network.HTTP.Client        (Response (..))
import           Network.HTTP.Types         (Status (..))
import           Servant.API
import           Servant.Client

import           OpenStack.Common

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
            ([ "identity" .= identity ] <> [ "scope" .= scope' | Just scope' <- pure scope ])
        domain :: Value
        domain = object [ "name" .= ("Default" :: String) ]
        identity :: Value
        identity = object
            [ "methods" .= [ "password" :: String ]
            , "password" .= object [ "user" .= passwordCredentials ] ]
        passwordCredentials :: Value
        passwordCredentials = object
            [ "name" .= (x ^. requestUsername)
            , "password" .= (x ^. requestPassword)
            , "domain" .= domain ]
        scope :: Maybe Value
        scope = do
            project <- x ^. requestTenantName
            pure $ object [ "project" .= object [ "domain" .= domain, "name" .= project ] ]

data Domain = Domain
    { _domainId          :: DomainId
    , _domainLinks       :: [Link]
    , _domainDescription :: Maybe Text
    , _domainEnabled     :: Maybe Bool
    , _domainName        :: Maybe Text
    } deriving (Eq, Show)

instance FromJSON Domain where
    parseJSON (Object o) = Domain
        <$> o .: "id"
        <*> o .: "links"
        <*> o .:? "description"
        <*> o .:? "enabled"
        <*> o .:? "name"
    parseJSON _ = mempty

instance FromJSON [Domain] where
    parseJSON (Object o) = do
        x <- o .: "domains"
        case x of
            Just (Array o') -> mapM parseJSON $ V.toList o'
            _ -> mempty
    parseJSON _ = mempty

type KeystoneApi =
         "v3" :> "auth" :> "tokens" :> ReqBody '[JSON] TokenRequest :> Raw
    :<|> "v3" :> "domains" :> Header "X-Auth-Token" TokenId :> Get '[JSON] [Domain]

keystoneApi :: Proxy KeystoneApi
keystoneApi = Proxy

data KeystoneMethods = KeystoneMethods
    { requestToken :: TokenRequest -> ExceptT ServantError IO TokenId
    , listDomains  :: TokenId -> ExceptT ServantError IO [Domain]
    }

keystoneMethods :: String -> Either String KeystoneMethods
keystoneMethods url = do
    baseUrl <- parseBaseUrl url
    let (requestToken' :<|>
         listDomains') = client keystoneApi
        requestToken req = coerce $ do
            (status,body,ct,res) <- requestToken' req "POST" baseUrl
            unless (status == 201) $ throwError $ FailureResponse (Status status "") ct body
            case lookup "X-Subject-Token" $ responseHeaders res of
                Nothing -> throwError $ DecodeFailure "X-Subject-Token not present" ct body
                Just x -> return $ TokenId x
        listDomains token =
            coerce $ listDomains' (Just token) baseUrl
    return KeystoneMethods{..}
