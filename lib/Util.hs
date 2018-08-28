module Util(createAppFolders, safeIO) where

import System.Directory(getAppUserDataDirectory, createDirectoryIfMissing)
import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Exception(handle, SomeException)

createAppFolders :: IO (FilePath, FilePath)
createAppFolders = do
  mininkBase <- getAppUserDataDirectory "minink"
  let lessonPath = mininkBase ++ "/lessons/"
  let dbPath = mininkBase ++ "/subscriptions.db"
  createDirectoryIfMissing True lessonPath
  return (lessonPath, dbPath)

safeIO :: MonadIO m => IO a -> m (Either String a)
safeIO action = liftIO $ handle catchAll $ Right <$> action

catchAll :: SomeException -> IO (Either String a)
catchAll = return . Left . show
