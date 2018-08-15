{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import MininkSend
import Time
import EmailSender
import Subscription
import SubscriptionDb
import Control.Monad.State.Strict (State, runState)
import Control.Monad.State (MonadState, modify, get)
import Test.Hspec
import qualified Data.Text.Lazy as LT

data MockState = MockState
  { subscriptionsInDb :: Either String [Subscription]
  , emailsSentOut :: [(String, LT.Text)]
  , currentTime :: Integer
  }

newtype MockSender a = MockSender {run :: State MockState a} deriving (Functor, Applicative, Monad, MonadState MockState)

instance EmailSender MockSender where
  sendEmail address content = do
    modify $ \s@(MockState _ emails _) -> s {emailsSentOut = emails ++ [(address, content)]}
    return $ Right ()

instance SubscriptionDb MockSender where
  loadSubscriptions = do
    (MockState subs _ _) <- get
    return subs

instance Epoch MockSender where
  currentTimeInEpoch = do
    (MockState _ _ currentTime) <- get
    return currentTime

runMock :: MockState -> (Either String (), MockState)
runMock initState = runState (run sendDailyMails) initState

main :: IO ()
main = hspec $ do
  describe "sending mail" $ do
    it "should send email to all subscribers" $ do
      let (result, MockState _ emails _) = runMock $ MockState (Right [Subscription 0 0 "a@b.com", Subscription 0 0 "b@c.com"]) [] 100000
      let addresses = fst <$> emails
      result `shouldBe` (Right ())
      addresses `shouldBe` ["a@b.com", "b@c.com"]

    it "should send email to a subscriber only once a day" $ do
      let (result, MockState _ emails _) = runMock $ MockState (Right [Subscription 0 0 "a@b.com", Subscription 100000 100000 "b@c.com"]) [] 100000
      let addresses = fst <$> emails
      result `shouldBe` (Right ())
      addresses `shouldBe` ["a@b.com"]
