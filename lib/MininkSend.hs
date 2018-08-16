{-# LANGUAGE OverloadedStrings #-}

module MininkSend(sendDailyMails) where

import EmailSender
import SubscriptionDb
import Subscription
import Time
import Data.DateTime (fromSeconds, toGregorian')
import Data.Either (either, lefts)

import Control.Monad (void)

areOnSameDay :: Integer -> Integer -> Bool
areOnSameDay t1 t2 = (toDayPrecision t1) == (toDayPrecision t2)
  where toDayPrecision :: Integer -> (Integer, Int, Int)
        toDayPrecision = toGregorian'.fromSeconds

shouldUpdate :: Integer -> Subscription -> Bool
shouldUpdate currentTime (Subscription _ lastSent _) = not $ areOnSameDay currentTime lastSent

send :: EmailSender m => Subscription -> m (Either String ())
send (Subscription _ _ subscriber) = sendEmail subscriber ""

sendEmailForToday :: (SubscriptionDb m, EmailSender m) => Integer -> [Subscription] -> m [Either String ()]
sendEmailForToday currentTime subs = do
  let updatableSubscriptions = filter (shouldUpdate currentTime) subs
  mapM sendAndUpdate updatableSubscriptions
  where sendAndUpdate subs = do
          sendResult <- (send subs)
          case sendResult of
            Left errMsg -> return sendResult
            Right _ -> updateSubscription (update subs)
        update s@(Subscription phase _ _) = s {phaseS = phase + 1, lastSentS = currentTime}

handleDbError :: Monad m => String -> m [Either String ()]
handleDbError errorMsg = return $ [Left errorMsg]

sendDailyMails :: (Epoch m, EmailSender m, SubscriptionDb m) => m (Either [String] ())
sendDailyMails = do
  currentTime <- currentTimeInEpoch
  maybeSubs <- loadSubscriptions
  results <- either handleDbError (sendEmailForToday currentTime) maybeSubs
  let errorMessages = lefts results
  return $
    if null errorMessages
    then Right ()
    else Left errorMessages
