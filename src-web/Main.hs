{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Email as Email
import WebApp
import Util(initApp)
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

data Config = Config
  { dbFile :: String
  , emailCredentials :: Email.Credentials
  }

newtype MininkWeb m a =
  MininkWeb (ReaderT Config m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

instance WebDb (MininkWeb IO) Config where
  saveConsent (Address addr) time = do
    dbPath <- asks dbFile
    result <-loggedSQL dbPath $ \conn -> do
      SQL.execute conn "INSERT INTO consents VALUES (?, ?)" (addr, time)
    return $ maybe (Left "Unable to save consent") (Right ()) result

  --saveRequest :: Address -> Token -> m (Either String ())
  --getRequest :: Token -> m (Either String Address)
  --deleteRequest :: Token -> m (Either String ())
  --saveSubscription :: Subscription -> m (Either String ())
  --runDb :: d -> m a -> IO a

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
  (_, dbPath) <- initApp
  let config = Config dbPath (Email.Credentials "" "")
  run 8081 (webApp "hasintro2018" config)

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

