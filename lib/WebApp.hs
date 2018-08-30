{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module WebApp where

import SubscriptionRequest
import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H
--import Network.Wai.Handler.Warp
--import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
--import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as BSL
--import qualified Data.ByteString.Char8 as BSC
--import qualified Data.ByteString.Base64.URL as URL

type SubscriptionApi =
  "subscription" :> ReqBody '[FormUrlEncoded] SubscriptionRequest :> Post '[HTML] H.Html :<|>
  Get '[HTML] H.Html :<|>
  "confirm" :> QueryParam "code" String :> Get '[HTML] H.Html :<|>
  "contact" :> Get '[HTML] H.Html

subscriptionApi :: Proxy SubscriptionApi
subscriptionApi = Proxy

subscriptionServer :: FilePath -> Server SubscriptionApi
subscriptionServer _ = requestSubs :<|> startPage :<|> confirm :<|> contact
  where requestSubs :: SubscriptionRequest -> Handler H.Html
        requestSubs = undefined
        startPage :: Handler H.Html
        startPage = undefined
        confirm :: Maybe String -> Handler H.Html
        confirm = undefined
        contact :: Handler H.Html
        contact = undefined

app :: FilePath -> Application
app dbFile = serve subscriptionApi (subscriptionServer dbFile)
