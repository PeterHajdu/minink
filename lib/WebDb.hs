module WebDb(WebDb(..)) where

class Monad m => WebDb m d | m -> d, d -> m where
