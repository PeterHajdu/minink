{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Util
import qualified Email as Email
import MininkSend
import LessonDb
import Time
import EmailSender
import Subscription
import SubscriptionDb
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS

import Control.Exception.Base (bracket)
import Data.DateTime (getCurrentTime, toSeconds)

import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple (NamedParam(..))

import System.IO (FilePath, hPutStrLn, stderr)
import System.Exit (exitFailure, exitSuccess)

import Options.Applicative hiding (action)
import Data.Semigroup ((<>))

import System.Directory (doesFileExist)

data Options = Options
  { emailCredentialsO :: Email.Credentials
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> Email.credentialParser

data Config = Config
  { subsDbConnection :: SQL.Connection
  , lessonBase :: String
  , emailCredentials :: Email.Credentials
  }

newtype Sender a = Sender
  { run :: ReaderT Config IO a
  } deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO)

instance EmailSender Sender where
  sendEmail address content = do
    credentials <- asks emailCredentials
    liftIO $ Email.send credentials (address) "peter@minink.io" "minink daily" content

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
        maybeContent <- safeIO $ BS.readFile fileName
        return $ Lesson <$> maybeContent
      else return $ Right $ Finished

runSender :: Config -> IO (Either [String] ())
runSender config = runReaderT (run sendDailyMails) config

printError :: String -> IO ()
printError = hPutStrLn stderr

handleResults :: Either [String] () -> IO ()
handleResults (Left errors) = mapM_ printError errors >> exitFailure
handleResults (Right _) = exitSuccess

withSQL :: FilePath -> (SQL.Connection -> IO ()) -> IO ()
withSQL dbPath = bracket (SQL.open dbPath) SQL.close

main :: IO ()
main = do
  options <- execParser $ info optionsParser (fullDesc <> progDesc "minink-send")
  (lessonPath, dbPath) <- initApp
  withSQL dbPath $ \dbConn -> do
    let config = Config dbConn lessonPath (emailCredentialsO options)
    result <- runSender config
    handleResults result
