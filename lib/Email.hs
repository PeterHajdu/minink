{-# LANGUAGE OverloadedStrings #-}

module Email(Credentials(..), send, credentialParser) where

import Util
import Mail.Hailgun
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

import Options.Applicative (Parser, strOption, long)

data Credentials = Credentials
  { domainC :: !String
  , apiKeyC :: !String
  }

credentialParser :: Parser Credentials
credentialParser = Credentials
  <$> strOption (long "domain")
  <*> strOption (long "apikey")

toHailgunContext :: Credentials -> HailgunContext
toHailgunContext (Credentials domain apiKey)= HailgunContext domain apiKey Nothing

send :: Credentials -> String -> String -> T.Text -> BS.ByteString -> IO (Either String ())
send cred recipient from subject content =
  let context = toHailgunContext cred
      maybeMessage = hailgunMessage subject (TextAndHTML "" content) (BSC.pack from) (emptyMessageRecipients {recipientsTo = [BSC.pack recipient]}) []
  in case maybeMessage of
       Left err -> return $ Left err
       Right msg -> do
         sendResult <- sendEmail context msg
         case sendResult of
           Left err -> return $ Left $ show err
           Right _ -> return $ Right ()
