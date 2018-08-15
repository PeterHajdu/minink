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

longAgo :: Integer
longAgo = 0

today :: Integer
today = 100000

data MockState = MockState
  { subscriptionsInDb :: Either String [Subscription]
  , emailsSentOut :: [(String, LT.Text)]
  }

newtype MockSender a = MockSender {run :: State MockState a} deriving (Functor, Applicative, Monad, MonadState MockState)

instance EmailSender MockSender where
  sendEmail address content = do
    modify $ \s@(MockState _ emails) -> s {emailsSentOut = emails ++ [(address, content)]}
    return $ Right ()

instance SubscriptionDb MockSender where
  loadSubscriptions = do
    (MockState subs _) <- get
    return subs

instance Epoch MockSender where
  currentTimeInEpoch = return today

runMock :: MockState -> (Either String (), MockState)
runMock initState = runState (run sendDailyMails) initState

a1 :: String
a1 = "a@b.com"

a2 :: String
a2 = "b@c.com"

runWithSubsAndEpoch :: Either String [Subscription] -> (Either String (), MockState)
runWithSubsAndEpoch subs = runMock $ MockState subs []

needsMail :: String -> Subscription
needsMail address = Subscription 0 longAgo address

doesNotNeedMail :: String -> Subscription
doesNotNeedMail address = Subscription 0 today address

main :: IO ()
main = hspec $ do
  describe "sending mail" $ do
    it "should send email to all subscribers" $ do
      let (result, MockState _ emails) = runWithSubsAndEpoch (Right [needsMail a1, needsMail a2])
      let addresses = fst <$> emails
      result `shouldBe` (Right ())
      addresses `shouldBe` [a1, a2]

    it "should send email to a subscriber only once a day" $ do
      let (result, MockState _ emails) = runWithSubsAndEpoch (Right [needsMail a1, doesNotNeedMail a2])
      let addresses = fst <$> emails
      result `shouldBe` (Right ())
      addresses `shouldBe` [a1]
