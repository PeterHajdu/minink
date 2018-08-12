{-# LANGUAGE OverloadedStrings #-}
module Main where

import Subscription (Subscription(..))
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.DateTime (fromSeconds, toGregorian', getCurrentTime, toSeconds)

sendEmail :: Integer -> Connection -> Subscription -> IO ()
sendEmail today conn subs = do
  print subs
  executeNamed conn "UPDATE subscription SET phase = :phase, lastsent = :lastsent WHERE address = :address" [":phase" := (phaseS subs) + 1, ":lastsent" := today, ":address" := (addressS subs)]

haveNotSentToday :: (Integer, Int, Int) -> Subscription -> Bool
haveNotSentToday today (Subscription _ lastSent _) = let date = toGregorian' $ fromSeconds lastSent
                                                     in date /= today

main :: IO ()
main = do
  conn <- open "/home/hptr/.minink/subscriptions.db"
  subscriptions <- query_ conn "SELECT * from subscription" :: IO [Subscription]
  today <- getCurrentTime
  let gregorian = toGregorian' today
  let shouldSend = filter (haveNotSentToday gregorian) subscriptions
  mapM_ (sendEmail (toSeconds today) conn) shouldSend
  close conn
