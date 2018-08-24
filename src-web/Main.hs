{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Servant
import Servant.HTML.Blaze
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class (liftIO)
import qualified Database.SQLite.Simple as SQL
import Web.FormUrlEncoded (FromForm)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data SubscriptionRequest = SubscriptionRequest
  { address :: !String
  } deriving (Generic, Show)

instance FromForm SubscriptionRequest

type SubscriptionApi =
  "subscription" :> ReqBody '[FormUrlEncoded] SubscriptionRequest :> Post '[HTML] NoContent :<|>
  "subscription" :> Get '[HTML] H.Html

dbFile :: FilePath
dbFile = "/home/hptr/.minink/subscriptions.db"

subscriptionServer :: Server SubscriptionApi
subscriptionServer = post :<|> get
  where post :: SubscriptionRequest -> Handler NoContent
        post request = do
          liftIO $ SQL.withConnection dbFile $ \conn -> do
            SQL.execute conn "INSERT INTO requests VALUES (?)" $ SQL.Only (address request)
          return NoContent
        get :: Handler H.Html
        get = return form
        form :: H.Html
        form = H.docTypeHtml $ do
          H.head $ H.title "somethingsomething"
          H.body $ H.form H.! A.method "post" H.! A.action "/subscription" $ do
            H.input H.! A.type_ "text" H.! A.name "address" H.! A.placeholder "email address"
            H.input H.! A.type_ "submit" H.! A.value "Send"


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
