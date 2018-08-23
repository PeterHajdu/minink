{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Servant
import Data.Aeson.Types
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class (liftIO)
import qualified Database.SQLite.Simple as SQL

data SubscriptionRequest = SubscriptionRequest
  { address :: String
  } deriving (Generic, Show)

instance FromJSON SubscriptionRequest

type SubscriptionApi = "subscription" :> ReqBody '[JSON] SubscriptionRequest :> Post '[JSON] NoContent

dbFile :: FilePath
dbFile = "/home/hptr/.minink/subscriptions.db"

subscriptionServer :: Server SubscriptionApi
subscriptionServer = subscribe
  where subscribe :: SubscriptionRequest -> Handler NoContent
        subscribe request = do
          liftIO $ SQL.withConnection dbFile $ \conn -> do
            SQL.execute conn "INSERT INTO requests VALUES (?)" $ SQL.Only (address request)
          return NoContent

subscriptionApi :: Proxy SubscriptionApi
subscriptionApi = Proxy

app :: Application
app = serve subscriptionApi subscriptionServer

initDb :: FilePath -> IO ()
initDb dbfile = SQL.withConnection dbfile $ \conn ->
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS requests (address text not null)"

main :: IO ()
main = do
  initDb dbFile
  run 8081 app
