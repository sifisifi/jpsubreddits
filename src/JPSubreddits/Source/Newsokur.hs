{-# LANGUAGE OverloadedStrings #-}
module JPSubreddits.Source.Newsokur where

import           Control.Applicative
import           Control.Monad.IO.Class (MonadIO)
import           Data.Maybe             (listToMaybe, mapMaybe)
import qualified Data.Text              as T
import           JPSubreddits.Types
import           Network.HTTP.Conduit   (simpleHttp)
import qualified Text.HTML.DOM          as HTML
import qualified Text.XML               as XML
import           Text.XML.Cursor        (($/), ($//), (&//))
import qualified Text.XML.Cursor        as XML

-- newsokurのwikiからスクレイピングでリストを取得
-- div.md.wiki下にある各tableに次の処理を行う
--
--  - 最初thのテキスト要素をカテゴリタイトルとする
--  - 以降のtdに含まれるa要素のhref属性をURL、テキスト要素をサブレ名として抽出
--    ただしhref属性の値がサブレのURLでない場合は無視する。
--
-- URL判定の方法
-- prefixが/r/
-- それ以降の最初の/又は終端までに文字列が存在する。
--
-- とりあえず取得してくるコード書く

data Newsokur = Newsokur
    deriving (Show, Read)

instance DataSource Newsokur where
    getListFromDataSource _ = getListFromNewsokur

getListFromNewsokur :: (Functor m, MonadIO m) => m [(CategoryName, [(Title, Url)])]
getListFromNewsokur = extract <$> getWiki

getWiki :: (Functor m, MonadIO m) => m XML.Document
getWiki = HTML.parseLBS <$> simpleHttp "http://www.reddit.com/r/newsokur/wiki/simple_list_ja"

extract :: XML.Document -> [(CategoryName, [(Title, Url)])]
extract doc = map category tables
  where
    c = XML.fromDocument doc
    tables = c $// XML.attributeIs "class" "md wiki" &// XML.element "table"

-- | tableへのCursorを渡す
category :: XML.Cursor -> (CategoryName, [(Title, Url)])
category c = (CategoryName cn, ss)
  where
    cn = maybe "unknown" (\c' -> T.concat $ c' $/ XML.content) $ listToMaybe $ c $// XML.element "th"
    ss = mapMaybe subreddit $ c $// XML.element "td" &// XML.element "a"

-- | aへのCursorを渡す
subreddit :: XML.Cursor -> Maybe (Title, Url)
subreddit c
    | isSubredditUrl u = Just (t, dropSlash u)
    | otherwise        = Nothing
  where
    u = Url $ T.concat $ XML.attribute "href" c
    t = Title $ T.concat $ c $/ XML.content

-- | subredditへの相対パスであるかどうかチェックする
isSubredditUrl :: Url -> Bool
isSubredditUrl (Url u) = "/r/" `T.isPrefixOf` u

-- | 末尾に/があれば取り除く
dropSlash :: Url -> Url
dropSlash = Url . T.dropWhileEnd (== '/') . unUrl

