{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module WebApp(subscriptionApi, webApp) where

import HtmlContent(confirmationEmail)
import Time
import EmailSender
import TokenGenerator
import WebDb
import SubscriptionRequest
import Subscription(initSubscription)
import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H
import Control.Monad.IO.Class
import Control.Monad(void)

type SubscriptionApi =
  "subscription" :> ReqBody '[FormUrlEncoded] SubscriptionRequest :> Post '[HTML] H.Html :<|>
  Get '[HTML] H.Html :<|>
  "confirm" :> QueryParam "code" String :> Get '[HTML] H.Html :<|>
  "contact" :> Get '[HTML] H.Html

subscriptionApi :: Proxy SubscriptionApi
subscriptionApi = Proxy

subscriptionServer :: (EmailSender m, TokenGenerator m, Epoch m, WebDb m d) => String -> d -> Server SubscriptionApi
subscriptionServer validCode dbContext = requestSubs :<|> startPage :<|> confirm :<|> contact
  where requestSubs :: SubscriptionRequest -> Handler H.Html
        requestSubs (SubscriptionRequest addr invCode maybeConsent) =
          if validCode == invCode
          then do
            maybe
              (return ())
              (const $ request $ Address addr)
              maybeConsent
            return site
          else return site

        request :: MonadIO m => Address -> m ()
        request addr@(Address strAddr) =
          void $ liftIO $ runDb dbContext $ do
            currentTimeInEpoch >>= saveConsent addr
            confirmationToken <- generateToken
            sendEmail strAddr (confirmationEmail confirmationToken)
            saveRequest addr confirmationToken

        confirm :: Maybe String -> Handler H.Html
        confirm (Just tokenStr) = do
          liftIO $ runDb dbContext $ do
            let token = Token tokenStr
            maybeAddress <- getRequest token
            either
              (const $ return ())
              startSubscription
              maybeAddress
            deleteRequest token
          return site
        confirm Nothing = return site

        startSubscription :: WebDb m d => Address -> m ()
        startSubscription (Address addr) = do
          saveSubscription $ initSubscription addr
          return ()

        startPage :: Handler H.Html
        startPage = undefined
        contact :: Handler H.Html
        contact = undefined

site :: H.Html
site = H.docTypeHtml $ do return ()

webApp :: (EmailSender m, TokenGenerator m, Epoch m, WebDb m d) => String -> d -> Application
webApp invCode dbContext = serve subscriptionApi (subscriptionServer invCode dbContext)
