module Subscription(Subscription(..), SubscriptionRequest(..)) where

data SubscriptionRequest = SubscriptionRequest
  { srsecretSR :: String
  , sraddressSR :: String
  }

data Subscription = Subscription
  { phaseS :: Int
  , lastSentS :: Integer
  , addressS :: String
  }

