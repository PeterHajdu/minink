{-# LANGUAGE OverloadedStrings #-}
module Main where

import Subscription (Subscription(..))
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.DateTime (fromSeconds, toGregorian', getCurrentTime, toSeconds)

import Network.HaskellNet.SMTP
import Network.HaskellNet.Auth
import Network.HaskellNet.SMTP.SSL
import qualified Data.Text.Lazy as T

sendEmail :: SMTPConnection -> Integer -> Connection -> Subscription -> IO ()
sendEmail smtpConn today conn subs = do
  print subs
  sendPlainTextMail (addressS subs) "peter.ferenc.hajdu@gmail.com" "minink daily mail" (T.pack $ show subs) smtpConn
  executeNamed conn "UPDATE subscription SET phase = :phase, lastsent = :lastsent WHERE address = :address" [":phase" := (phaseS subs) + 1, ":lastsent" := today, ":address" := (addressS subs)]

haveNotSentToday :: (Integer, Int, Int) -> Subscription -> Bool
haveNotSentToday today (Subscription _ lastSent _) = let date = toGregorian' $ fromSeconds lastSent
                                                     in date /= today

main :: IO ()
main = do
  smtpConn <- connectSMTPSTARTTLSWithSettings "smtp.gmail.com" (Settings 587 0 True True)
  result <- authenticate LOGIN "peter.ferenc.hajdu@gmail.com" "tcepjzibgwoqcwwy" smtpConn

  conn <- open "/home/hptr/.minink/subscriptions.db"
  subscriptions <- query_ conn "SELECT * from subscription" :: IO [Subscription]
  today <- getCurrentTime
  let gregorian = toGregorian' today
  let shouldSend = filter (haveNotSentToday gregorian) subscriptions
  mapM_ (sendEmail smtpConn (toSeconds today) conn) shouldSend
  close conn
