module Subscription(Subscription(..), initSubscription) where

import Database.SQLite.Simple.FromRow

initSubscription :: String -> Subscription
initSubscription addr = Subscription 0 0 addr

data Subscription = Subscription
  { phaseS :: Int
  , lastSentS :: Integer
  , addressS :: String
  } deriving (Show, Eq)

instance FromRow Subscription where
  fromRow = Subscription <$> field <*> field <*> field
