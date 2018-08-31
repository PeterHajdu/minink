{-# LANGUAGE OverloadedStrings #-}

module HtmlContent(confirmationEmail) where

import TokenGenerator

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

confirmationEmail :: Token -> BS.ByteString
confirmationEmail (Token strToken) =
  let htmlMail = H.docTypeHtml $ do
                   H.head $ do
                     H.title "Subscription confirmation to the Introduction to Haskell course"
                     H.meta H.! A.charset "utf-8"
                   H.body $ do
                     H.h1 $ do "Subscription confirmation to the Introduction to Haskell course"
                     H.a H.! A.href (H.toValue $ "https://minink.io/confirm?code=" ++ strToken) $ do
                       "Yes, subscribe me to this course."
                     H.p $ do "If you received this email by mistake, simply delete it. You won't be subscribed \
                     \ if you don't click the confirmation link above."
  in BSL.toStrict $ renderHtml htmlMail
