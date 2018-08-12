{-# LANGUAGE OverloadedStrings #-}
module Main where

import Subscription (Subscription(..))
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

main :: IO ()
main = do
  conn <- open "/home/hptr/.minink/subscriptions.db"
  r <- query_ conn "SELECT * from subscription" :: IO [Subscription]
  mapM_ print r
  close conn
