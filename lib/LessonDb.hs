module LessonDb(LessonDb(..), RetrieveResult(..)) where

import Subscription
import qualified Data.Text.Lazy as LT

data RetrieveResult =
    Lesson LT.Text
  | Finished

class Monad m => LessonDb m where
  retrieveLesson :: Subscription -> m (Either String RetrieveResult)
