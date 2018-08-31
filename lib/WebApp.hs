{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module WebApp(subscriptionApi, webApp) where

import qualified HtmlContent as Page
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
              (return Page.noConsent)
              (const $ request $ Address addr)
              maybeConsent
          else return Page.invalidInvitationCode

        request :: MonadIO m => Address -> m H.Html
        request addr@(Address strAddr) = do
          liftIO $ runDb dbContext $ do
            currentTimeInEpoch >>= saveConsent addr
            confirmationToken <- generateToken
            sendEmail strAddr (Page.confirmationEmail confirmationToken)
            saveRequest addr confirmationToken
          return Page.pleaseConfirm

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
          return Page.confirmSuccess
        confirm Nothing = return Page.confirmSuccess

        startSubscription :: WebDb m d => Address -> m ()
        startSubscription (Address addr) = do
          saveSubscription $ initSubscription addr
          return ()

        startPage :: Handler H.Html
        startPage = return Page.startPage

        contact :: Handler H.Html
        contact = return Page.contact

webApp :: (EmailSender m, TokenGenerator m, Epoch m, WebDb m d) => String -> d -> Application
webApp invCode dbContext = serve subscriptionApi (subscriptionServer invCode dbContext)
