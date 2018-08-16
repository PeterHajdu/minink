{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import MininkSend
import LessonDb
import Time
import EmailSender
import Subscription
import SubscriptionDb
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.Lazy as LT

import Network.HaskellNet.SMTP
import Network.HaskellNet.Auth
import Network.HaskellNet.SMTP.SSL
import Control.Exception (handle, SomeException)
import Data.DateTime (getCurrentTime, toSeconds)

import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple (NamedParam(..))
import qualified Data.Text.IO as TIO

import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing, doesFileExist)
import System.IO (FilePath, hPutStrLn, stderr)
import System.Exit (exitFailure, exitSuccess)

data Config = Config
  { smtpConnection :: SMTPConnection
  , subsDbConnection :: SQL.Connection
  , lessonBase :: String
  }

newtype Sender a = Sender {run :: ReaderT Config IO a} deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO)

instance EmailSender Sender where
  sendEmail address content = do
    connection <- asks smtpConnection
    safeIO $ sendMimeMail address "peter.ferenc.hajdu@gmail.com" "minink daily mail" "" content [] connection

instance SubscriptionDb Sender where
  loadSubscriptions = do
    dbConn <- asks subsDbConnection
    safeIO $ SQL.query_ dbConn "SELECT * from subscription"

  updateSubscription subs = do
    dbConn <- asks subsDbConnection
    let query = "UPDATE subscription SET phase = :phase, lastsent = :lastsent WHERE address = :address"
    let params = [":phase" := (phaseS subs) + 1, ":lastsent" := (lastSentS subs), ":address" := (addressS subs)]
    safeIO $ SQL.executeNamed dbConn query params


instance Epoch Sender where
  currentTimeInEpoch = do
    date <- liftIO getCurrentTime
    return $ toSeconds date


instance LessonDb Sender where
  retrieveLesson (Subscription phase _ _) = do
    lessonBaseFolder <- asks lessonBase
    let fileName = lessonBaseFolder ++ (show phase)
    doesExist <- liftIO $ doesFileExist fileName
    if doesExist
      then do
        maybeContent <- safeIO $ TIO.readFile fileName
        return $ (Lesson . LT.fromStrict) <$> maybeContent
      else return $ Right $ Finished

runSender :: Config -> IO (Either [String] ())
runSender config = runReaderT (run sendDailyMails) config

createAppFolders :: IO (FilePath, FilePath)
createAppFolders = do
  mininkBase <- getAppUserDataDirectory "minink"
  let lessonPath = mininkBase ++ "/lessons/"
  let dbPath = mininkBase ++ "/subscriptions.db"
  createDirectoryIfMissing True lessonPath
  return (lessonPath, dbPath)

printError :: String -> IO ()
printError = hPutStrLn stderr

main :: IO ()
main = do
  (lessonPath, dbPath) <- createAppFolders
  smtpConn <- connectSMTPSTARTTLSWithSettings "smtp.gmail.com" (Settings 587 0 True True)
  authenticate LOGIN "peter.ferenc.hajdu@gmail.com" "tcepjzibgwoqcwwy" smtpConn
  dbConn <- SQL.open dbPath
  let config = Config smtpConn dbConn lessonPath
  result <- runSender config
  SQL.close dbConn
  closeSMTP smtpConn
  case result of
    Left errors -> do
      mapM_ printError errors
      exitFailure
    Right _ -> exitSuccess

safeIO :: MonadIO m => IO a -> m (Either String a)
safeIO action = liftIO $ handle catchAll $ Right <$> action

catchAll :: SomeException -> IO (Either String a)
catchAll = return . Left . show
