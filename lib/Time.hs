module Time(Epoch(..)) where

class Monad m => Epoch m where
  currentTimeInEpoch :: m Integer

