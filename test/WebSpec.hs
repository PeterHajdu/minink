{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WebSpec(webSpec) where

import Data.List
import Test.Hspec

import TokenGenerator
import EmailSender
import Time
import WebApp
import WebDb
import Network.Wai.Handler.Warp(testWithApplication)
import Servant.Client

import Servant.API
import Servant.HTML.Blaze
import Network.HTTP.Client

import Control.Exception(throwIO, ErrorCall(..))
import Control.Monad.Trans.Except(runExceptT)

import Control.Monad (void)

import WebApp(subscriptionApi)
import SubscriptionRequest(SubscriptionRequest(..))
import Subscription(Subscription(..))

import qualified Text.Blaze.Html5 as H

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import Control.Monad.Trans.Reader hiding (ask)
import Control.Monad.Reader.Class
import Control.Monad.IO.Class

instance MimeUnrender HTML a where
  mimeUnrender _ _ = Left "irrelevant"

subscribe :: SubscriptionRequest -> ClientM H.Markup
confirmSubscription :: Maybe String -> ClientM H.Markup
startPage :: ClientM H.Markup
contact :: ClientM H.Markup

subscribe :<|> startPage :<|> confirmSubscription :<|> contact  = client subscriptionApi

properRequest :: SubscriptionRequest
properRequest = SubscriptionRequest "harcsa.bajusz@gmail.com" "hasIntro2018" (Just "")

requestWithNoConsent :: SubscriptionRequest
requestWithNoConsent = properRequest {consent = Nothing}

requestWithInvalidInvitationCode:: SubscriptionRequest
requestWithInvalidInvitationCode = properRequest {invitationCode = "invalid code"}

currentTime :: Integer
currentTime = 424242

generatedTokenStr :: String
generatedTokenStr = "harcsabajusz"

generatedToken :: Token
generatedToken = Token generatedTokenStr

webSpec =
  around withApp $ do
    describe "the web app" $ do
      describe "request subscription" $ do
        it "should save the request to the db" $ \(stmDb, port') -> do
          try port' (subscribe properRequest)
          db <- liftIO $ readTVarIO stmDb
          (length $ requests db) `shouldBe` 1
          let request = head $ requests db
          (fst $ request) `shouldBe` (Address $ address properRequest)
          (snd $ request) `shouldBe` generatedToken

        it "should save the consent to the db" $ \(stmDb, port') -> do
          try port' (subscribe properRequest)
          db <- liftIO $ readTVarIO stmDb
          (length $ consents db) `shouldBe` 1
          let consent = head $ consents db
          (fst consent) `shouldBe` (Address $ address properRequest)
          (snd consent) `shouldBe` (currentTime)

        it "should send the confirmation email to the subscriber" $ \(stmDb, port') -> do
          try port' (subscribe properRequest)
          db <- liftIO $ readTVarIO stmDb
          (length $ emailsSentTo db) `shouldBe` 1
          let email = head $ emailsSentTo db
          return ()

        it "should not save request if the consent is not checked in" $ \(stmDb, port') -> do
          try port' (subscribe requestWithNoConsent)
          db <- liftIO $ readTVarIO stmDb
          (length $ requests db) `shouldBe` 0
          (length $ consents db) `shouldBe` 0
          (length $ emailsSentTo db) `shouldBe` 0

        it "should not save request if the invitation code is invalid" $ \(stmDb, port') -> do
          try port' (subscribe requestWithInvalidInvitationCode)
          db <- liftIO $ readTVarIO stmDb
          (length $ requests db) `shouldBe` 0
          (length $ consents db) `shouldBe` 0
          (length $ emailsSentTo db) `shouldBe` 0

      describe "subscription confirmation" $ do
        it "should start the subscription" $ \(stmDb, port') -> do
          try port' (subscribe properRequest)
          try port' (confirmSubscription $ Just generatedTokenStr)
          db <- liftIO $ readTVarIO stmDb
          (length $ subscriptions db) `shouldBe` 1
          let subscription = head $ subscriptions db
          phaseS subscription `shouldBe` 0
          lastSentS subscription `shouldBe` 0
          addressS subscription `shouldBe` (address properRequest)

        it "should delete the request" $ \(stmDb, port') -> do
          try port' (subscribe properRequest)
          try port' (confirmSubscription $ Just generatedTokenStr)
          db <- liftIO $ readTVarIO stmDb
          (length $ requests db) `shouldBe` 0

        it "should handle invalid confirmation codes" $ \(_, port') -> do
          try port' (confirmSubscription Nothing)
          try port' (confirmSubscription $ Just "invalid token")

      describe "static content" $ do
        it "should serve contact information" $ \(_, port') -> do
          try port' contact

        it "should serve the start page" $ \(_, port') -> do
          try port' startPage

data Db = Db
  { requests :: [(Address, Token)]
  , subscriptions :: [Subscription]
  , consents :: [(Address, Integer)]
  , emailsSentTo :: [Address]
  }

type Table = TVar Db

newtype Mock m a =
  Mock (ReaderT Table m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Table)

modDb :: (MonadIO m, MonadReader Table m) => (Db -> Db) -> m ()
modDb mod = do
  table <- ask
  liftIO $ atomically $ do
    db <- readTVar table
    writeTVar table (mod db)

instance EmailSender (Mock IO) where
  sendEmail addr mail = do
    modDb $ \db@(Db _ _ _ emails) -> db {emailsSentTo = (Address addr):emails}
    return $ Right ()

instance TokenGenerator (Mock IO) where
  generateToken = return generatedToken

instance Epoch (Mock IO) where
  currentTimeInEpoch = return currentTime

instance WebDb (Mock IO) Table where
  saveRequest addr token = do
    modDb $ \db@(Db req _ _ _) -> db {requests = (addr, token):req}
    return $ Right ()

  saveConsent addr timeStamp = do
    modDb $ \db@(Db _ _ cons _) -> db {consents = (addr, timeStamp):cons}
    return $ Right ()

  saveSubscription sub = do
    modDb $ \db@(Db _ subs _ _) -> db {subscriptions = sub:subs}
    return $ Right ()

  getRequest token = do
    table <- ask
    db <- liftIO $ readTVarIO table
    let maybeRequest = find (\req -> (snd req) == token) (requests db)
    return $ maybe (Left "not found") (Right . fst) maybeRequest

  deleteRequest token = do
    modDb $ \db@(Db reqs _ _ _) -> db {requests = filter (\(_, t) -> t /= token) reqs}
    return $ Right ()

  runDb db (Mock readert)  = runReaderT readert db

withApp :: ((Table, Int) -> IO ()) -> IO ()
withApp testBody = do
  db <- atomically $ newTVar (Db [] [] [] [])
  testWithApplication (return $ webApp (invitationCode properRequest) db) (\port -> testBody (db, port))

try :: Int -> ClientM a -> IO ()
try port' action = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port' ""
  let env = mkClientEnv manager baseUrl
  void $ runClientM action env
