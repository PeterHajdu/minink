module LessonDb(LessonDb(..), RetrieveResult(..)) where

import Subscription
import qualified Data.ByteString as BS

data RetrieveResult =
    Lesson BS.ByteString
  | Finished

class Monad m => LessonDb m where
  retrieveLesson :: Subscription -> m (Either String RetrieveResult)
