{-# LANGUAGE OverloadedStrings #-}

module MininkSend(sendDailyMails) where

import LessonDb
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

send :: EmailSender m => RetrieveResult -> Subscription -> m (Either String ())
send (Lesson content) (Subscription _ _ subscriber) = sendEmail subscriber content
send Finished _ = return $ Right ()

sendEmailForToday :: (LessonDb m, SubscriptionDb m, EmailSender m) => Integer -> [Subscription] -> m [Either String ()]
sendEmailForToday currentTime subs =
  let updatableSubscriptions = filter (shouldUpdate currentTime) subs
  in mapM sendAndUpdate updatableSubscriptions

  where sendAndUpdate subs = do
          lessonResult <- retrieveLesson subs
          case lessonResult of
            Left errMsg -> return $ Left errMsg
            Right lesson -> sendLesson subs lesson

        sendLesson subs lesson = do
          sendResult <- send lesson subs
          case sendResult of
            Left errMsg -> return sendResult
            Right _ -> updateSubscription (update subs)

        update s@(Subscription phase _ _) = s {phaseS = phase + 1, lastSentS = currentTime}

handleDbError :: Monad m => String -> m [Either String ()]
handleDbError errorMsg = return $ [Left errorMsg]

sendDailyMails :: (LessonDb m, Epoch m, EmailSender m, SubscriptionDb m) => m (Either [String] ())
sendDailyMails = do
  currentTime <- currentTimeInEpoch
  maybeSubs <- loadSubscriptions
  results <- either handleDbError (sendEmailForToday currentTime) maybeSubs
  let errorMessages = lefts results
  return $
    if null errorMessages
    then Right ()
    else Left errorMessages
