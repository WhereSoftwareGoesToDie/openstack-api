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

data CreateUserRequest = CreateUserRequest
    { _createUserRequestDefaultProject :: ProjectId
    , _createUserRequestDescription    :: Text
    , _createUserRequestDomainId       :: DomainId
    , _createUserRequestEmail          :: Text
    , _createUserRequestEnabled        :: Bool
    , _createUserRequestName           :: Text
    , _createUserRequestPassword       :: Text
    } deriving (Eq, Show)
makeLenses ''CreateUserRequest

instance ToJSON CreateUserRequest where
    toJSON CreateUserRequest{..} = object [ "user" .= user ]
      where
        user = object
            [ "default_project_id" .= _createUserRequestDefaultProject
            , "description" .= _createUserRequestDescription
            , "domain_id" .= _createUserRequestDomainId
            , "email" .=  _createUserRequestEmail
            , "enabled" .= _createUserRequestEnabled
            , "name" .= _createUserRequestName
            , "password" .= _createUserRequestPassword
            ]

data CreateUserResponse = CreateUserResponse
    { _createUserResponseDefaultProject :: ProjectId
    , _createUserResponseDescription    :: Text
    , _createUserResponseDomainId       :: DomainId
    , _createUserResponseEmail          :: Text
    , _createUserResponseEnabled        :: Bool
    , _createUserResponseName           :: Text
    , _createUserResponseId             :: UserId
    } deriving (Eq, Show)
makeLenses ''CreateUserResponse

instance FromJSON CreateUserResponse where
    parseJSON (Object o) = do
        (Object o') <- o .: "user"
        CreateUserResponse
            <$> o' .: "default_project_id"
            <*> o' .: "description"
            <*> o' .: "domain_id"
            <*> o' .: "email"
            <*> o' .: "enabled"
            <*> o' .: "name"
            <*> o' .: "id"
    parseJSON _ = mempty

data CreateProjectRequest = CreateProjectRequest
    { _createProjectRequestDescription :: Maybe Text
    , _createProjectRequestDomain      :: DomainId
    , _createProjectRequestEnabled     :: Bool
    , _createProjectRequestName        :: Text
    } deriving (Eq, Show)
makeLenses ''CreateProjectRequest

instance ToJSON CreateProjectRequest where
    toJSON CreateProjectRequest{..} = object [ "project" .= project ]
      where
        project = object
            [ "description" .= _createProjectRequestDescription
            , "domain_id" .= _createProjectRequestDomain
            , "enabled" .= _createProjectRequestEnabled
            , "name" .= _createProjectRequestName
            ]

data CreateProjectResponse = CreateProjectResponse
    { _createProjectResponseDescription :: Maybe Text
    , _createProjectResponseDomain      :: DomainId
    , _createProjectResponseEnabled     :: Bool
    , _createProjectResponseName        :: Text
    , _createProjectResponseId          :: ProjectId
    } deriving (Eq, Show)
makeLenses ''CreateProjectResponse

instance FromJSON CreateProjectResponse where
    parseJSON (Object o) = do
        (Object o') <- o .: "project"
        CreateProjectResponse
            <$> o' .: "description"
            <*> o' .: "domain_id"
            <*> o' .: "enabled"
            <*> o' .: "name"
            <*> o' .: "id"
    parseJSON _ = mempty

type KeystoneApi =
         "v3" :> "auth" :> "tokens" :> ReqBody '[JSON] TokenRequest :> Raw
    :<|> "v3" :> "domains" :> Header "X-Auth-Token" TokenId :> Get '[JSON] [Domain]
    :<|> "v3" :> "projects" :> ReqBody '[JSON] CreateProjectRequest :> Header "X-Auth-Token" TokenId :> Post '[JSON] CreateProjectResponse
    :<|> "v3" :> "projects" :> Capture "project_id" ProjectId :> Header "X-Auth-Token" TokenId :> Delete
    :<|> "v3" :> "users" :> ReqBody '[JSON] CreateUserRequest :> Header "X-Auth-Token" TokenId :> Post '[JSON] CreateUserResponse
    :<|> "v3" :> "users" :> Capture "user_id" UserId :> Header "X-Auth-Token" TokenId :> Delete

keystoneApi :: Proxy KeystoneApi
keystoneApi = Proxy

data KeystoneMethods = KeystoneMethods
    { requestToken  :: TokenRequest -> ExceptT ServantError IO TokenId
    , listDomains   :: TokenId -> ExceptT ServantError IO [Domain]
    , createProject :: TokenId -> CreateProjectRequest -> ExceptT ServantError IO CreateProjectResponse
    , deleteProject :: TokenId -> ProjectId -> ExceptT ServantError IO ()
    , createUser    :: TokenId -> CreateUserRequest -> ExceptT ServantError IO CreateUserResponse
    , deleteUser    :: TokenId -> UserId -> ExceptT ServantError IO ()
    }

keystoneMethods :: String -> Either String KeystoneMethods
keystoneMethods url = do
    baseUrl <- parseBaseUrl url
    let (requestToken' :<|>
         listDomains' :<|>
         createProject' :<|>
         deleteProject' :<|>
         createUser' :<|>
         deleteUser') = client keystoneApi
        requestToken req = coerce $ do
            (status,body,ct,res) <- requestToken' req "POST" baseUrl
            unless (status == 201) $ throwError $ FailureResponse (Status status "") ct body
            case lookup "X-Subject-Token" $ responseHeaders res of
                Nothing -> throwError $ DecodeFailure "X-Subject-Token not present" ct body
                Just x -> return $ TokenId x
        listDomains token =
            coerce $ listDomains' (Just token) baseUrl
        createProject token req =
            coerce $ createProject' req (Just token) baseUrl
        deleteProject token pid =
            coerce $ deleteProject' pid (Just token) baseUrl
        createUser token req =
            coerce $ createUser' req (Just token) baseUrl
        deleteUser token user =
            coerce $ deleteUser' user (Just token) baseUrl
    return KeystoneMethods{..}
