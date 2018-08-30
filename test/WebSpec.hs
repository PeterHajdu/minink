{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WebSpec(webSpec) where

import Test.Hspec

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
startPageGet :: ClientM H.Markup
contact :: ClientM H.Markup

subscribe :<|> startPageGet :<|> confirmSubscription :<|> contact  = client subscriptionApi

properRequest :: SubscriptionRequest
properRequest = SubscriptionRequest "harcsa.bajusz@gmail.com" "hasIntro2018" (Just "")

webSpec =
  around withApp $ do
    describe "the web app" $ do
      describe "request subscription" $ do
        it "should save the request to the db" $ \(stmDb, port') -> do
          try port' (subscribe properRequest)
          db <- liftIO $ readTVarIO stmDb
          (length $ requests db) `shouldBe` 1


data Db = Db
  { requests :: [(Address, Token)]
  , subscriptions :: [Subscription]
  , consents :: [(Address, Integer)]
  }

type Table = TVar Db

newtype DbMock m a =
  DbMock (ReaderT Table m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Table)

instance WebDb (DbMock IO) Table where
  saveRequest addr token = do
    table <- ask
    liftIO $ atomically $ do
      db@(Db req _ _) <- readTVar table
      let newDb = db {requests = (addr, token):req}
      writeTVar table newDb
    return $ Right ()

  --saveConsent :: Address -> Integer -> m (Either String ())
  --getRequest :: Token -> m (Either String Address)
  --deleteRequest :: Token -> m (Either String ())
  --saveSubscription :: Subscription -> m (Either String ())
  --runDb :: d -> m a -> IO a

  --getTraining trainingTitle = do
  --  table <- ask
  --  result <- liftIO $ atomically $ do
  --    (trainings, _) <- readTVar table
  --    return $ lookup trainingTitle trainings
  --  return result

  --addTraining training = do
  --  table <- ask
  --  result <- liftIO $ atomically $ do
  --    (trainings, subs) <- readTVar table
  --    writeTVar table ((trainingId training, training):trainings, subs)
  --    return $ Just $ trainingId training
  --  return $ result

  --getTrainings = do
  --  table <- ask
  --  result <- liftIO $ atomically $ do
  --    (trainings, _) <- readTVar table
  --    return trainings
  --  return $ snd <$> result

  --addSubscription subscription = do
  --  table <- ask
  --  result <- liftIO $ atomically $ do
  --    (trainings, subs) <- readTVar table
  --    writeTVar table (trainings, subscription:subs)
  --    return $ Just $ subscribedTraining subscription
  --  return $ result

  runDb db (DbMock readert)  = liftIO $ (runReaderT readert db)


withApp :: ((Table, Int) -> IO ()) -> IO ()
withApp testBody = do
  db <- atomically $ newTVar (Db [] [] [])
  testWithApplication (return $ webApp db) (\port -> testBody (db, port))

try :: Int -> ClientM a -> IO ()
try port' action = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port' ""
  let env = mkClientEnv manager baseUrl
  void $ runClientM action env
