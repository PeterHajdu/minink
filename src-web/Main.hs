{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Maybe (listToMaybe)
import GHC.Generics
import Servant
import Servant.HTML.Blaze
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class (liftIO)
import qualified Database.SQLite.Simple as SQL
import Web.FormUrlEncoded (FromForm)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64.URL as URL
import System.IO (withBinaryFile, IOMode(ReadMode))

data SubscriptionRequest = SubscriptionRequest
  { address :: !String
  , invitationCode :: !String
  } deriving (Generic, Show)

instance FromForm SubscriptionRequest

type SubscriptionApi =
  "subscription" :> ReqBody '[FormUrlEncoded] SubscriptionRequest :> Post '[HTML] NoContent :<|>
  "subscription" :> Get '[HTML] H.Html :<|>
  "confirm" :> QueryParam "code" String :> Get '[HTML] NoContent

dbFile :: FilePath
dbFile = "/home/hptr/.minink/subscriptions.db"

subscriptionServer :: Server SubscriptionApi
subscriptionServer = post :<|> get :<|> confirm
  where post :: SubscriptionRequest -> Handler NoContent
        post request = if (invitationCode request == "hasintro2018")
                       then liftIO $ subscribe $ address request
                       else return NoContent
        get :: Handler H.Html
        get = return form
        form :: H.Html
        form = H.docTypeHtml $ do
          H.head $ H.title "somethingsomething"
          H.body $ H.form H.! A.method "post" H.! A.action "/subscription" $ do
            H.input H.! A.type_ "text" H.! A.name "address" H.! A.placeholder "email address"
            H.input H.! A.type_ "text" H.! A.name "invitationCode" H.! A.placeholder "invitation code"
            H.input H.! A.type_ "submit" H.! A.value "Send"
        subscribe :: String -> IO NoContent
        subscribe addr = do
          token <- generateToken
          SQL.withConnection dbFile $ \conn -> do
            SQL.setTrace conn (Just print)
            SQL.execute conn "INSERT INTO requests VALUES (?, ?)" (addr, BSC.unpack token)
          return NoContent
        confirm :: Maybe String -> Handler NoContent
        confirm (Just code) = do
          maybeAddress <- liftIO $ retrieveSubscriptionRequest code
          maybe (return NoContent) confirmSubscription maybeAddress
        confirm _ = do
          return NoContent
        retrieveSubscriptionRequest :: String -> IO (Maybe String)
        retrieveSubscriptionRequest code = do
          SQL.withConnection dbFile $ \conn -> do
            results <- SQL.query conn "SELECT address from requests where token=?" (SQL.Only code)
            return $ (listToMaybe (results :: [[String]])) >>= listToMaybe
        confirmSubscription :: String -> Handler NoContent
        confirmSubscription addr = do
          liftIO $ SQL.withConnection dbFile $ \conn -> do
            SQL.setTrace conn (Just print)
            SQL.execute conn "INSERT INTO subscription VALUES (?, ?, ?)" (0::Int, 0::Int, addr)
            SQL.execute conn "DELETE FROM requests where address=?" (SQL.Only addr)
          return NoContent

subscriptionApi :: Proxy SubscriptionApi
subscriptionApi = Proxy

app :: Application
app = serve subscriptionApi subscriptionServer

initDb :: FilePath -> IO ()
initDb dbfile = SQL.withConnection dbfile $ \conn ->
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS requests (address text not null, token varchar not null)"

generateToken :: IO BS.ByteString
generateToken = withBinaryFile "/dev/urandom" ReadMode $ \handle -> do
  binToken <- BS.hGet handle 64
  return $ URL.encode binToken

main :: IO ()
main = do
  initDb dbFile
  run 8081 app
