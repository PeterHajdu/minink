module EmailSender(EmailSender(..)) where

import qualified Data.ByteString as BS

class Monad m => EmailSender m where
  sendEmail :: String -> BS.ByteString -> m (Either String ())

