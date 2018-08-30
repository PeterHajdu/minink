{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module WebSpec(webSpec) where

import Test.Hspec

import WebApp
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

import qualified Text.Blaze.Html5 as H

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

instance MimeUnrender HTML a where
  mimeUnrender _ _ = Left "irrelevant"

subscribe :: SubscriptionRequest -> ClientM H.Markup
confirmSubscription :: Maybe String -> ClientM H.Markup
startPageGet :: ClientM H.Markup
contact :: ClientM H.Markup

subscribe :<|> startPageGet :<|> confirmSubscription :<|> contact  = client subscriptionApi

properRequest :: SubscriptionRequest
properRequest = SubscriptionRequest "harcsa.bajusz@gmail.com" "hasIntro2018" (Just "")

webSpec =
  around withApp $ do
    describe "the web app" $ do
      describe "request subscription" $ do
        it "should save the request to the db" $ \port' -> do
          try port' (subscribe properRequest)
          True `shouldBe` False

withApp :: (Int -> IO a) -> IO a
withApp testBody = do
  --table <- atomically $ newTVar (initialDb, [] :: [Subscription])
  --application <- mkApp table
  --testWithApplication (return application) action
  testWithApplication (return (webApp "alma")) testBody

try :: Int -> ClientM a -> IO ()
try port' action = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port' ""
  let env = mkClientEnv manager baseUrl
  void $ runClientM action env
