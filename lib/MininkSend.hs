{-# LANGUAGE OverloadedStrings #-}

module MininkSend(sendDailyMails) where

import EmailSender
import SubscriptionDb
import Subscription
import Time
import Data.DateTime (fromSeconds, toGregorian')

import Control.Monad (void)

areOnSameDay :: Integer -> Integer -> Bool
areOnSameDay t1 t2 = (toDayPrecision t1) == (toDayPrecision t2)
  where toDayPrecision :: Integer -> (Integer, Int, Int)
        toDayPrecision = toGregorian'.fromSeconds

send :: EmailSender m => Integer -> Subscription -> m ()
send currentTime (Subscription _ lastSent subscriber) =
  if areOnSameDay currentTime lastSent then return () else void $ sendEmail subscriber ""

sendDailyMails :: (Epoch m, EmailSender m, SubscriptionDb m) => m (Either String ())
sendDailyMails = do
  (Right subs) <- loadSubscriptions
  currentTime <- currentTimeInEpoch
  mapM_ (send currentTime) subs
  return $ Right ()

