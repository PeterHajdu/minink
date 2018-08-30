{-# LANGUAGE DeriveGeneric #-}

module SubscriptionRequest(SubscriptionRequest(..)) where

import GHC.Generics
import Web.FormUrlEncoded (FromForm)

data SubscriptionRequest = SubscriptionRequest
  { address :: !String
  , invitationCode :: !String
  , consent :: !(Maybe String)
  } deriving (Generic, Show)

instance FromForm SubscriptionRequest

