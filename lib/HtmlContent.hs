{-# LANGUAGE OverloadedStrings #-}

module HtmlContent
  ( confirmSuccess
  , confirmationEmail
  , startPage
  , contact
  , confirmSuccess
  , invalidInvitationCode
  , noConsent
  , pleaseConfirm) where

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

startPage :: H.Html
startPage = site $ do
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

contact :: H.Html
contact = site $ do "info@minink.io"

confirmSuccess :: H.Html
confirmSuccess = site $ do "Your subscription has been confirmed.  You should shortly receive your first lecture."

invalidInvitationCode :: H.Html
invalidInvitationCode = site $ do "We are sorry but that is an invalid invitation code.  Just email us and we will send you a valid one."

noConsent :: H.Html
noConsent = site $ do "We can not enroll you without your consent."

pleaseConfirm :: H.Html
pleaseConfirm = site $ do "A confirmation email has been sent to your address.  Please confirm by clicking on the link in the email."
