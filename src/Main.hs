module Main where

import           Control.Applicative
import           Control.Monad.IO.Class       (MonadIO)
import qualified Data.Aeson                   as A
import qualified Data.Aeson.Encode.Pretty     as AP
import qualified Data.ByteString.Lazy.Char8   as LBS
import           Data.Time                    (getZonedTime)
import           JPSubreddits.Source.Newsokur
import           JPSubreddits.Subreddit
import           JPSubreddits.Types

main :: IO ()
main = do
    xs <- getListFromDataSource Newsokur
    ss <- mapM toCategory xs
    time <- getZonedTime
    let jpss = JPSubreddits time ss
    LBS.writeFile "jpsubreddits.json" $ A.encode jpss
    LBS.writeFile "jpsubreddits-pretty.json" $ encodePretty jpss
    putStrLn "Done."

toCategory :: (Functor m, MonadIO m) => (CategoryName, [(Title, Url)]) -> m Category
toCategory (cn, xs) = Category cn <$> mapM (uncurry getSubredditData) xs

encodePretty :: A.ToJSON a => a -> LBS.ByteString
encodePretty = AP.encodePretty' config
  where
    config = AP.Config 2 compare

