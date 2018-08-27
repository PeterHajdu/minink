{-# LANGUAGE OverloadedStrings #-}

module Email(Credentials(..), send) where

import Mail.Hailgun
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

data Credentials = Credentials
  { domainC :: String
  , apiKeyC :: String
  }

toHailgunContext :: Credentials -> HailgunContext
toHailgunContext = undefined

send :: Credentials -> String -> String -> T.Text -> BS.ByteString -> IO ()
send cred recipient from subject content =
  let context = toHailgunContext cred
      maybeMessage = hailgunMessage subject (TextAndHTML "" content) (BSC.pack from) (emptyMessageRecipients {recipientsTo = [BSC.pack recipient]}) []
  in case maybeMessage of
       Left err -> putStrLn err
       Right msg -> void $ sendEmail context msg
