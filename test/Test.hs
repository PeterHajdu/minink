module Main where

import Test.Hspec
import SenderSpec
import WebSpec

main :: IO ()
main = hspec $ do
  senderSpec
  webSpec
