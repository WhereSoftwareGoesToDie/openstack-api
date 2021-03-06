{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module OpenStack.Common where

import           Control.Applicative
import           Control.Lens.TH
import           Data.Aeson
import           Data.ByteString     (ByteString)
import qualified Data.HashMap.Strict as H
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Data.Time.Clock
import           Data.Time.Format
import qualified Data.Vector         as V
import           Servant.API
import           System.Locale

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

newtype TenantId = TenantId
    { unTenantId :: ByteString
    } deriving (Eq, Show)
makeWrapped ''TenantId

instance FromJSON TenantId where
    parseJSON x = TenantId . T.encodeUtf8 <$> parseJSON x

instance ToJSON TenantId where
    toJSON = toJSON . T.decodeUtf8 . unTenantId

instance ToText TenantId where
    toText = T.decodeUtf8 . unTenantId

newtype TokenId = TokenId
    { unTokenId :: ByteString
    } deriving (Eq, Show)
makeWrapped ''TokenId

instance FromJSON TokenId where
    parseJSON x = TokenId . T.encodeUtf8 <$> parseJSON x

instance ToJSON TokenId where
    toJSON = toJSON . T.decodeUtf8 . unTokenId

instance ToText TokenId where
    toText = T.decodeUtf8 . unTokenId

newtype UserId = UserId
    { unUserId :: ByteString
    } deriving (Eq, Show)
makeWrapped ''UserId

instance FromJSON UserId where
    parseJSON x = UserId . T.encodeUtf8 <$> parseJSON x

instance ToJSON UserId where
    toJSON = toJSON . T.decodeUtf8 . unUserId

instance ToText UserId where
    toText = T.decodeUtf8 . unUserId

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

newtype DomainId = DomainId
    { unDomainId :: ByteString
    } deriving (Eq, Show)
makeWrapped ''DomainId

instance FromJSON DomainId where
    parseJSON x = DomainId . T.encodeUtf8 <$> parseJSON x

instance ToJSON DomainId where
    toJSON = toJSON . T.decodeUtf8 . unDomainId

instance ToText DomainId where
    toText = T.decodeUtf8 . unDomainId

newtype ProjectId = ProjectId
    { unProjectId :: ByteString
    } deriving (Eq, Show)
makeWrapped ''ProjectId

instance FromJSON ProjectId where
    parseJSON x = ProjectId . T.encodeUtf8 <$> parseJSON x

instance ToJSON ProjectId where
    toJSON = toJSON . T.decodeUtf8 . unProjectId

instance ToText ProjectId where
    toText = T.decodeUtf8 . unProjectId

data Link = Link
    { _linkHref :: Text
    , _linkRel  :: Text
    } deriving (Eq, Show)

instance FromJSON Link where
    parseJSON (Object o) = Link
        <$> o .: "href"
        <*> o .: "rel"
    parseJSON _ = mempty

instance FromJSON [Link] where
    parseJSON (Object o) = mapM (\(k,v) -> Link k <$> parseJSON v) $ H.toList o
    parseJSON (Array xs) = mapM parseJSON $ V.toList xs
    parseJSON _ = mempty
