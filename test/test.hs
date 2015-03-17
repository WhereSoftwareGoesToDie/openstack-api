{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time.Clock
import           Data.Time.Format
import           System.Environment
import           System.Locale
import           Test.Hspec
import           Test.HUnit

import           OpenStack.Common
import           OpenStack.Keystone

makeupname :: MonadIO m => Text -> m Text
makeupname prefix = do
    t <- liftIO getCurrentTime
    return $ prefix <> "-" <> (T.pack $ formatTime defaultTimeLocale "%s%q" t)

suite :: Text -> Text -> KeystoneMethods -> Spec
suite username password KeystoneMethods{..} =
    describe "Keystone" $ do
        it "can issue a token" $ do
            res <- runExceptT $ requestToken (TokenRequest (Just "demo") username password)
            case res of
                Right _ -> return ()
                Left e -> assertFailure $ show e

        it "can ask for a list of domains" $ do
            res <- runExceptT $ do
                tid <- requestToken (TokenRequest (Just "demo") username password)
                listDomains tid
            case res of
                Left e -> assertFailure $ show e
                Right ds -> case filter (\Domain{..} -> _domainName == Just "Default") ds of
                    []  -> assertFailure "No \"Default\" domain"
                    (_:_) -> return ()

        it "can create and delete a project" $ do
            res <- runExceptT $ do
                tid <- requestToken (TokenRequest (Just "demo") username password)
                let _createProjectRequestDescription = Nothing
                    _createProjectRequestDomain = DomainId "default"
                    _createProjectRequestEnabled = True
                _createProjectRequestName <- makeupname "project"
                CreateProjectResponse{..} <- createProject tid CreateProjectRequest{..}
                deleteProject tid _createProjectResponseId
            case res of
                Left e -> assertFailure $ show e
                Right () -> return ()

        it "can create and delete a user" $ do
            res <- runExceptT $ do
                tid <- requestToken (TokenRequest (Just "demo") username password)
                let _createProjectRequestDescription = Nothing
                    _createProjectRequestDomain = DomainId "default"
                    _createProjectRequestEnabled = True
                _createProjectRequestName <- makeupname "project"
                CreateProjectResponse{..} <- createProject tid CreateProjectRequest{..}
                let _createUserRequestDefaultProject = _createProjectResponseId
                    _createUserRequestDescription = "desc"
                    _createUserRequestDomainId = DomainId "default"
                    _createUserRequestEmail = "foo@foo.de"
                    _createUserRequestEnabled = True
                    _createUserRequestPassword = "password"
                _createUserRequestName <- makeupname "user"
                CreateUserResponse{..} <- createUser tid CreateUserRequest{..}
                deleteUser tid _createUserResponseId
                deleteProject tid _createProjectResponseId
            case res of
                Left e -> assertFailure $ show e
                Right _ -> return ()

main :: IO ()
main = do
    username <- T.pack <$> getEnv "OS_USERNAME"
    password <- T.pack <$> getEnv "OS_PASSWORD"
    keystoneUrl <- getEnv "KEYSTONE_URL"
    keystone <- case keystoneMethods keystoneUrl of
        Left e -> error e
        Right x -> return x
    hspec $ suite username password keystone
