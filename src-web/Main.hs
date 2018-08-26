{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import System.Environment (getArgs)
import qualified Email as Email
import Data.Maybe (listToMaybe)
import GHC.Generics
import Servant
import Servant.HTML.Blaze
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class (liftIO)
import qualified Database.SQLite.Simple as SQL
import Web.FormUrlEncoded (FromForm)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64.URL as URL
import System.IO (withBinaryFile, IOMode(ReadMode))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Text.Lazy.Encoding (decodeUtf8)

data SubscriptionRequest = SubscriptionRequest
  { address :: !String
  , invitationCode :: !String
  , consent :: !(Maybe String)
  } deriving (Generic, Show)

instance FromForm SubscriptionRequest

type SubscriptionApi =
  "subscription" :> ReqBody '[FormUrlEncoded] SubscriptionRequest :> Post '[HTML] H.Html :<|>
  Get '[HTML] H.Html :<|>
  "confirm" :> QueryParam "code" String :> Get '[HTML] H.Html :<|>
  "contact" :> Get '[HTML] H.Html

dbFile :: FilePath
dbFile = "/home/hptr/.minink/subscriptions.db"

subscriptionServer :: Server SubscriptionApi
subscriptionServer = post :<|> get :<|> confirm :<|> contact
  where post :: SubscriptionRequest -> Handler H.Html
        post request = do
          if (invitationCode request == "hasintro2018")
          then case consent request of
                 Nothing -> return $ site $ do "We can not enroll you without your consent."
                 Just _ -> liftIO $ subscribe $ address request
          else return $ site $ do "We are sorry but that is an invalid invitation code.  Just email us and we will send you a valid one."

        get :: Handler H.Html
        get = return form

        form :: H.Html
        form = enrollForm

        subscribe :: String -> IO H.Html
        subscribe addr = do
          token <- generateToken
          currentTime <- getPOSIXTime
          SQL.withConnection dbFile $ \conn -> do
            SQL.execute conn "INSERT INTO consents VALUES (?, ?)" (addr, (round currentTime :: Integer))
            SQL.execute conn "INSERT INTO requests VALUES (?, ?)" (addr, BSC.unpack token)
          sendConfirmationEmail token addr
          return $ site $ do "A confirmation email has been sent to your address.  Please confirm by clicking on the link in the email."

        sendConfirmationEmail :: BS.ByteString -> String -> IO ()
        sendConfirmationEmail token addr = do
          let email = H.docTypeHtml $ do
                        H.head $ do
                          H.title "Subscription confirmation to the Introduction to Haskell course"
                          H.meta H.! A.charset "utf-8"
                        H.body $ do
                          H.h1 $ do "Subscription confirmation to the Introduction to Haskell course"
                          H.a H.! A.href (H.toValue $ "https://minink.io/confirm?code=" ++ BSC.unpack token) $ do
                            "Yes, subscribe me to this course."
                          H.p $ do "If you received this email by mistake, simply delete it. You won't be subscribed \
                          \ if you don't click the confirmation link above."
          connection <- Email.connect
          let rawEmail = decodeUtf8 $ renderHtml email
          print rawEmail
          Email.send connection addr "Introduction to Haskell confirmation" rawEmail
          return ()

        confirm :: Maybe String -> Handler H.Html
        confirm (Just code) = do
          maybeAddress <- liftIO $ retrieveSubscriptionRequest code
          maybe (return confirmOk) confirmSubscription maybeAddress
        confirm _ = do
          return confirmOk

        retrieveSubscriptionRequest :: String -> IO (Maybe String)
        retrieveSubscriptionRequest code = do
          SQL.withConnection dbFile $ \conn -> do
            results <- SQL.query conn "SELECT address from requests where token=?" (SQL.Only code)
            return $ (listToMaybe (results :: [[String]])) >>= listToMaybe

        confirmSubscription :: String -> Handler H.Html
        confirmSubscription addr = do
          liftIO $ SQL.withConnection dbFile $ \conn -> do
            SQL.setTrace conn (Just print)
            SQL.execute conn "INSERT INTO subscription VALUES (?, ?, ?)" (0::Int, 0::Int, addr)
            SQL.execute conn "DELETE FROM requests where address=?" (SQL.Only addr)
          return confirmOk

        contact :: Handler H.Html
        contact = return $ site $ do "info@minink.io"

confirmOk :: H.Html
confirmOk = site $ do "Your subscription has been confirmed.  You should shortly receive your first lecture."

site :: H.Html -> H.Html
site content = H.docTypeHtml $ do
  H.head $ do
    H.title "minink"
    H.meta H.! A.charset "utf-8"
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1"
    H.link H.! A.rel "stylesheet" H.! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
  H.body $ do
    H.nav H.! A.class_ "navbar navbar-expand-sm bg-dark navbar-dark" $ do
      H.a H.! A.class_ "navbar-brand" H.! A.href "/" $ do
        "minink"
      H.ul H.! A.class_ "navbar-nav" $ do
        H.li H.! A.class_ "nav-item" $ do
          H.a H.! A.class_ "nav-link" H.! A.href "/contact" $ do "Contact"
    H.div H.! A.class_ "inner container align-middle w-50" H.! A.style "margin-top:30px" $ do content

enrollForm :: H.Html
enrollForm = site $ do
  H.h3 $ do "Introduction to Haskell"
  H.p H.! A.style "margin-top:30px" $ do
    "This course walks you through the basic language features in three \
    \ weeks.  A bite sized lecture is sent to you every \
    \ day with exercises.  If you need help you can simply reply to the \
    \ email containing the lecture.  Knowledge of an imperative language \
    \ such as Java might help but is not necessary.  All you need is a unix \
    \ like operating system, 15-20 minutes every day and dedication.  Enrollment \
    \ is free but an invitation code is required.  You can ask for one by emailing us."
  H.form H.! A.action "/subscription" H.! A.method "post" H.! A.style "margin-top:50px" $ do
    H.div H.! A.class_ "form-group" $ do
      H.label H.! A.for "address" $ do "Email address:"
      H.input H.! A.type_ "email" H.! A.class_ "form-control" H.! A.name "address"
    H.div H.! A.class_ "form-group" $ do
      H.label H.! A.for "invitationCode" $ do "Invitation code:"
      H.input H.! A.type_ "text" H.! A.class_ "form-control" H.! A.name "invitationCode"
      H.div H.! A.class_ "checkbox" $ do
        H.p $ do
          "Minink is commited to protecting and respecting your privacy, and we'll only use your personal information \
          \ to administer your subscription and to provide the services that you requested from us. \
          \ In order to provide you the content requested, we need to store and process your personal data.  If you consent \
          \ to us storing your personal data for this purpose, please tick the checkbox below."
        H.label H.! A.for "consent" $ do
          H.input H.! A.type_ "checkbox" H.! A.value "" H.! A.name "consent"
          "I agree to Minink's storage and processing of my personal data."
        H.p $ do "You may unsubscribe from the course or withdraw your consent at anytime by sending an email to us."
    H.button H.! A.type_ "submit" H.! A.class_ "btn btn-primary" $ do "Enroll"

subscriptionApi :: Proxy SubscriptionApi
subscriptionApi = Proxy

app :: Application
app = serve subscriptionApi subscriptionServer

initDb :: FilePath -> IO ()
initDb dbfile = SQL.withConnection dbfile $ \conn -> do
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS requests (address text not null, token varchar not null)"
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS consents (address text not null, long not null)"

generateToken :: IO BS.ByteString
generateToken = withBinaryFile "/dev/urandom" ReadMode $ \handle -> do
  binToken <- BS.hGet handle 64
  return $ URL.encode binToken

main :: IO ()
main = do
  args <- getArgs
  let port = (read $ head args) :: Int
  initDb dbFile
  run port app
