{-# LANGUAGE OverloadedStrings #-}

module Email(Connection, send, connect, close) where

import Network.HaskellNet.SMTP
import Network.HaskellNet.SMTP.SSL
import qualified Data.Text.Lazy as LT

newtype Connection = Connection SMTPConnection

send :: Connection -> String -> String -> LT.Text -> IO ()
send (Connection connection) address subject content =
  sendMimeMail address "peter.ferenc.hajdu@gmail.com" subject "" content [] connection

connect :: IO Connection
connect = do
  smtpConn <- connectSMTPSTARTTLSWithSettings "smtp.gmail.com" (Settings 587 0 True True)
  _ <- authenticate LOGIN "peter.ferenc.hajdu@gmail.com" "tcepjzibgwoqcwwy" smtpConn
  return $ Connection smtpConn

close :: Connection -> IO ()
close (Connection conn) = closeSMTP conn
