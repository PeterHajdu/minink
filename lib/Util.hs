{-# LANGUAGE OverloadedStrings #-}

module Util(initApp, safeIO) where

import System.Directory(getAppUserDataDirectory, createDirectoryIfMissing)
import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Exception(handle, SomeException)
import qualified Database.SQLite.Simple as SQL

initApp :: IO (FilePath, FilePath)
initApp = do
  mininkBase <- getAppUserDataDirectory "minink"
  let lessonPath = mininkBase ++ "/lessons/"
  let dbPath = mininkBase ++ "/subscriptions.db"
  initDb dbPath
  createDirectoryIfMissing True lessonPath
  return (lessonPath, dbPath)

initDb :: FilePath -> IO ()
initDb dbfile = SQL.withConnection dbfile $ \conn -> do
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS requests (address text not null, token varchar not null)"
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS consents (address text not null, long not null)"
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS subscription (phase int, lastsent long, address text)"

safeIO :: MonadIO m => IO a -> m (Either String a)
safeIO action = liftIO $ handle catchAll $ Right <$> action

catchAll :: SomeException -> IO (Either String a)
catchAll = return . Left . show
