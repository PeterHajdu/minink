{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module WebDb(WebDb(..), Address(..), Token(..)) where

import Subscription(Subscription(..))

newtype Address = Address String
newtype Token = Token String

class Monad m => WebDb m d | m -> d, d -> m where
  saveConsent :: Address -> Integer -> m (Either String ())
  saveRequest :: Address -> Token -> m (Either String ())
  getRequest :: Token -> m (Either String Address)
  deleteRequest :: Token -> m (Either String ())
  saveSubscription :: Subscription -> m (Either String ())
  runDb :: d -> m a -> IO a

