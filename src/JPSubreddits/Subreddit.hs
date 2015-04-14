module JPSubreddits.Subreddit
  ( getSubredditData
  ) where

import Control.Monad.IO.Class (MonadIO)
import JPSubreddits.Types

getSubredditData :: MonadIO m => Title -> Url -> m Subreddit
getSubredditData t u = return $ Subreddit u t

