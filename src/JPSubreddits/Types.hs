{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module JPSubreddits.Types where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson             (FromJSON (..), ToJSON (..), object, (.:), (.=))
import Data.Text              (Text)
import Data.Time              (ZonedTime)

class DataSource a where
    getListFromDataSource :: (Functor m, MonadIO m) => a -> m [(CategoryName, [(Title, Url)])]

newtype Url = Url { unUrl :: Text }
    deriving (Show, Read, ToJSON, FromJSON)

newtype Title = Title { unTitle :: Text }
    deriving (Show, Read, ToJSON, FromJSON)

newtype CategoryName = CategoryName { unCategoryName :: Text }
    deriving (Show, Read, ToJSON, FromJSON)

data Subreddit = Subreddit
    { url   :: Url
    , title :: Title
    }
    deriving (Show, Read)

data Category = Category
    { name       :: CategoryName
    , subreddits :: [Subreddit]
    }
    deriving (Show, Read)

data JPSubreddits = JPSubreddits
    { generatedAt :: ZonedTime
    , categories  :: [Category]
    }
    deriving (Show, Read)

---------------------------------------------
-- Instancies
---------------------------------------------

instance FromJSON Subreddit where
    parseJSON = parseJSON >=> go
      where
        go o = Subreddit
            <$> o .: "url"
            <*> o .: "title"

instance ToJSON Subreddit where
    toJSON s = object
        [ "url" .= url s
        , "title" .= title s
        ]

instance FromJSON Category where
    parseJSON = parseJSON >=> go
      where
        go o = Category
            <$> o .: "category"
            <*> o .: "list"

instance ToJSON Category where
    toJSON s = object
        [ "category" .= name s
        , "list" .= subreddits s
        ]

instance FromJSON JPSubreddits where
    parseJSON = parseJSON >=> go
      where
        go o = JPSubreddits
            <$> o .: "generated_at"
            <*> o .: "jpsubreddits"

instance ToJSON JPSubreddits where
    toJSON s = object
        [ "generated_at" .= show (generatedAt s)
        , "jpsubreddits" .= categories s
        ]

