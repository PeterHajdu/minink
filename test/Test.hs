{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import MininkSend
import Time
import EmailSender
import Subscription
import SubscriptionDb
import Control.Monad.State.Strict (State, runState)
import Control.Monad.State (MonadState, put, get)
import Test.Hspec
import qualified Data.Text.Lazy as LT

longAgo :: Integer
longAgo = 0

today :: Integer
today = 100000

data MockState = MockState
  { emailSendResults :: [Either String ()]
  , subscriptionsInDb :: Either String [Subscription]
  , emailsSentOut :: [(String, LT.Text)]
  , updatedSubscriptions :: [Subscription]
  }

newtype MockSender a = MockSender {run :: State MockState a} deriving (Functor, Applicative, Monad, MonadState MockState)

safeTail :: [a] -> [a]
safeTail (x:xs) = xs
safeTail [] = []

instance EmailSender MockSender where
  sendEmail address content = do
    s@(MockState results _ emails _) <- get
    put $ s {emailsSentOut = emails ++ [(address, content)], emailSendResults = safeTail results}
    return $ head results

instance SubscriptionDb MockSender where
  loadSubscriptions = do
    (MockState _ subs _ _) <- get
    return subs

  updateSubscription subs = do
    s@(MockState _ _ _ updatedSubs) <- get
    put $ s {updatedSubscriptions = updatedSubs ++ [subs]}
    return $ Right ()

instance Epoch MockSender where
  currentTimeInEpoch = return today

runMock :: MockState -> (Either [String] (), MockState)
runMock initState = runState (run sendDailyMails) initState

a1 :: String
a1 = "a@b.com"

a2 :: String
a2 = "b@c.com"

a3 :: String
a3 = "c@d.com"

allEmailSucceeds = repeat (Right ())

runWithSubs :: Either String [Subscription] -> (Either [String] (), MockState)
runWithSubs subs = runMock $ MockState allEmailSucceeds subs [] []

runWithSubsAndEmailError :: Either String [Subscription] -> [Either String ()] -> (Either [String] (), MockState)
runWithSubsAndEmailError subs emailresults = runMock $ MockState emailresults subs [] []

needsMail :: String -> Subscription
needsMail address = Subscription 0 longAgo address

nextPhase :: Subscription -> Subscription
nextPhase s@(Subscription oldPhase _ _) = s {phaseS = oldPhase + 1, lastSentS = today}

doesNotNeedMail :: String -> Subscription
doesNotNeedMail address = Subscription 0 today address

main :: IO ()
main = hspec $ do
  describe "sending mail" $ do
    it "should send email to all subscribers" $ do
      let (result, state) = runWithSubs (Right [needsMail a1, needsMail a2])
      let addresses = fst <$> (emailsSentOut state)
      result `shouldBe` (Right ())
      addresses `shouldBe` [a1, a2]

    it "should send email to a subscriber only once a day" $ do
      let (result, state) = runWithSubs (Right [needsMail a1, doesNotNeedMail a2])
      let addresses = fst <$> (emailsSentOut state)
      result `shouldBe` (Right ())
      addresses `shouldBe` [a1]

    it "should return if there's a db error" $ do
      let errorMsg = "unable to access db"
      let (result, _) = runWithSubs (Left errorMsg)
      result `shouldBe` (Left [errorMsg])

    it "should return errors from email sending" $ do
      let (result, _) = runWithSubsAndEmailError (Right [needsMail a1, needsMail a2, needsMail a3]) [Left "some error", Right (), Left "another error"]
      result `shouldBe` (Left ["some error", "another error"])

    it "should increment the phase of subscriptions where email sending succeeds" $ do
      let (s1, s2) = (needsMail a1, needsMail a2)
      let (result, state) = runWithSubsAndEmailError (Right [s1, s2]) [Right (), Left "some error"]
      (updatedSubscriptions state) `shouldBe` [nextPhase s1]
