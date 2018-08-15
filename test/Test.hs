module Main where

data MockState = MockState

data MockSender = MockSender {run :: State MockState ()} deriving ()

main :: IO ()
main = putStrLn "test"
