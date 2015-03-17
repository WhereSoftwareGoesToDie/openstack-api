{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Applicative
import           Control.Monad.Trans.Except
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           System.Environment
import           Test.Hspec

import           OpenStack.Keystone

suite :: Text -> Text -> KeystoneMethods -> Spec
suite username password KeystoneMethods{..} =
    describe "Keystone" $ do
        it "can issue a token" $ do
            res <- runExceptT $ requestToken (TokenRequest (Just "demo") username password)
            case res of
                Right _ -> return ()
                Left e -> fail $ show e

        it "can ask for a list of domains" $ do
            Right tid <- runExceptT $ requestToken (TokenRequest (Just "demo") username password)
            res <- runExceptT $ listDomains tid
            case res of
                Left e -> fail $ show e
                Right ds -> case filter (\Domain{..} -> _domainName == Just "Default") ds of
                    []  -> fail "No \"Default\" domain"
                    (_:_) -> return ()

main :: IO ()
main = do
    username <- T.pack <$> getEnv "OS_USERNAME"
    password <- T.pack <$> getEnv "OS_PASSWORD"
    keystoneUrl <- getEnv "KEYSTONE_URL"
    keystone <- case keystoneMethods keystoneUrl of
        Left e -> fail e
        Right x -> return x
    hspec $ suite username password keystone
