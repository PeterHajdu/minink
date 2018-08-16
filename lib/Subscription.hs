module Subscription(Subscription(..)) where

import Database.SQLite.Simple.FromRow

data Subscription = Subscription
  { phaseS :: Int
  , lastSentS :: Integer
  , addressS :: String
  } deriving (Show, Eq)

instance FromRow Subscription where
  fromRow = Subscription <$> field <*> field <*> field
