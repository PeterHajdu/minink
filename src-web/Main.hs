{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Subscription
import qualified Email as Email
import WebApp
import Util(safeSQL, initApp)
import WebDb
import EmailSender
import TokenGenerator

import qualified Database.SQLite.Simple as SQL

import System.IO (withBinaryFile, IOMode(ReadMode))
import qualified Data.ByteString.Base64.URL as URL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Time
import Data.Time.Clock.POSIX (getPOSIXTime)

import Network.Wai.Handler.Warp(run)

import Control.Monad.Trans.Reader hiding (asks)
import Control.Monad.Reader.Class
import Control.Monad.IO.Class (liftIO, MonadIO)

import System.Log.Logger
import System.Log.Handler.Syslog

import Data.Maybe (listToMaybe)
import Control.Monad (join)

import Options.Applicative hiding (action)
import Data.Semigroup ((<>))

data Config = Config
  { dbFile :: String
  , emailCredentials :: Email.Credentials
  , invitationCode :: String
  }

credentialParser :: Parser Email.Credentials
credentialParser = Email.Credentials
  <$> strOption (long "domain")
  <*> strOption (long "apikey")

configParser :: FilePath -> Parser Config
configParser dbPath = Config dbPath
  <$> credentialParser
  <*> strOption (long "invitationcode")

newtype MininkWeb m a =
  MininkWeb (ReaderT Config m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

instance WebDb (MininkWeb IO) Config where
  saveConsent (Address addr) time = do
    dbPath <- asks dbFile
    result <-loggedSQL dbPath $ \conn -> do
      SQL.execute conn "INSERT INTO consents VALUES (?, ?)" (addr, time)
    return $ maybe (Left "Unable to save consent") (const $ Right ()) result

  saveRequest (Address addr) (Token tok) = do
    dbPath <- asks dbFile
    result <-loggedSQL dbPath $ \conn -> do
      SQL.execute conn "INSERT INTO requests VALUES (?, ?)" (addr, tok)
    return $ maybe (Left "Unable to save request") (const $ Right ()) result

  getRequest (Token tok) = do
    dbPath <- asks dbFile
    maybeMaybe <- loggedSQL dbPath $ \conn -> do
      results <- SQL.query conn "SELECT address from requests where token=?" (SQL.Only tok)
      return $ (listToMaybe (results :: [[String]])) >>= listToMaybe
    return $ maybe (Left "Did not find request") (Right . Address) (join maybeMaybe)

  deleteRequest (Token tok) = do
    dbPath <- asks dbFile
    result <-loggedSQL dbPath $ \conn -> do
      SQL.execute conn "DELETE FROM requests where token=?" (SQL.Only tok)
    return $ maybe (Left "Unable to delete request") (const $ Right ()) result

  saveSubscription (Subscription phase lastSent addr) = do
    dbPath <- asks dbFile
    result <-loggedSQL dbPath $ \conn -> do
      SQL.execute conn "INSERT INTO subscription VALUES (?, ?, ?)" (phase, lastSent, addr)
    return $ maybe (Left "Unable to save subscription") (const $ Right ()) result

  runDb config (MininkWeb readert) = runReaderT readert config

instance EmailSender (MininkWeb IO) where
  sendEmail address content = do
    credentials <- asks emailCredentials
    liftIO $ Email.send credentials address "peter@minink.io" "minink.io confirmation" content

instance TokenGenerator (MininkWeb IO) where
  generateToken = liftIO $ withBinaryFile "/dev/urandom" ReadMode $ \handle -> do
    binToken <- BS.hGet handle 64
    return $ Token $ BSC.unpack $ URL.encode binToken

instance Epoch (MininkWeb IO) where
  currentTimeInEpoch = round <$> (liftIO getPOSIXTime)

main :: IO ()
main = do
  initializeLogger
  (_, dbPath) <- initApp
  config <- execParser $ info (configParser dbPath) ( fullDesc
       <> progDesc "minink"
       <> header "hello - a test for optparse-applicative" )

  debug "Started"
  run 8081 (webApp (invitationCode config) config)

initializeLogger :: IO ()
initializeLogger = do
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  s <- openlog "minink-web" [PID] USER DEBUG
  updateGlobalLogger rootLoggerName (addHandler s)

debug :: MonadIO m => String -> m ()
debug msg = liftIO $ infoM "minink-web" msg

errorLog :: MonadIO m => String -> m ()
errorLog msg = liftIO $ errorM "minink-web" msg

loggedSQL :: MonadIO m => FilePath -> (SQL.Connection -> IO a) -> m (Maybe a)
loggedSQL dbPath action = do
  result <- safeSQL dbPath action
  case result of
    Left err -> errorLog err >> return Nothing
    Right val -> return $ Just val

loggedSQL_ :: MonadIO m => FilePath -> (SQL.Connection -> IO a) -> m ()
loggedSQL_ db action = do
  _ <- loggedSQL db action
  return ()

