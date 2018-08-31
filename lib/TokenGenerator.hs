module TokenGenerator(TokenGenerator(..), Token(..)) where

newtype Token = Token String deriving (Show, Eq)

class Monad m => TokenGenerator m where
  generateToken :: m Token
