module SubscriptionDb(SubscriptionDb(..)) where

import Subscription

class Monad m => SubscriptionDb m where
  loadSubscriptions :: m (Either String [Subscription])
