module EmailSender(EmailSender(..)) where

import qualified Data.Text.Lazy as LT

class Monad m => EmailSender m where
  sendEmail :: String -> LT.Text -> m (Either String ())

