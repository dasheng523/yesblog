{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Rel8Example where

import Data.Time
import Hasql.Connection qualified as Connection
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config (Config, acquisitionTimeout, agingTimeout, idlenessTimeout, initSession, observationHandler, settings, size, staticConnectionSettings)
import Hasql.Session (statement)
import Rel8

-- | ArticleId 强类型
newtype ArticleId = ArticleId {unArticleId :: Text}
  deriving newtype (DBEq, DBType, Eq, Show, Read)

-- | TagId 强类型
newtype TagId = TagId {unTagId :: Text}
  deriving newtype (DBEq, DBType, Eq, Show)

-- | Article 表的 Rel8 映射
data Article f = Article
  { articleId :: Column f ArticleId
  , articleName :: Column f Text
  , articleContent :: Column f (Maybe Text)
  , articleCreatedAt :: Column f UTCTime
  , articleUpdatedAt :: Column f UTCTime
  , articleEnable :: Column f Bool
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- | Article 表的 TableSchema
articleSchema :: TableSchema (Article Name)
articleSchema =
  TableSchema
    { name = "article"
    , columns =
        Article
          { articleId = "id"
          , articleName = "name"
          , articleContent = "content"
          , articleCreatedAt = "created_at"
          , articleUpdatedAt = "updated_at"
          , articleEnable = "enable"
          }
    }

-- | Tag 表的 Rel8 映射
data Tag f = Tag
  { tagId :: Column f TagId
  , tagName :: Column f Text
  , tagCreatedAt :: Column f UTCTime
  , tagUpdatedAt :: Column f UTCTime
  , tagEnable :: Column f Bool
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- | Tag 表的 TableSchema
tagSchema :: TableSchema (Tag Name)
tagSchema =
  TableSchema
    { name = "tag"
    , columns =
        Tag
          { tagId = "id"
          , tagName = "name"
          , tagCreatedAt = "created_at"
          , tagUpdatedAt = "updated_at"
          , tagEnable = "enable"
          }
    }

-- | ArticleTag 关联表的 Rel8 映射 (用于联接查询，不直接作为 Rel8able 表)
data ArticleTag f = ArticleTag
  { articleTagArticleId :: Column f ArticleId
  , articleTagTagId :: Column f TagId
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- | ArticleTag 表的 TableSchema
articleTagSchema :: TableSchema (ArticleTag Name)
articleTagSchema =
  TableSchema
    { name = "article_tag"
    , columns =
        ArticleTag
          { articleTagArticleId = "article_id"
          , articleTagTagId = "tag_id"
          }
    }

-- | Show 实例，用于打印查询结果
deriving stock instance (f ~ Result) => Show (Article f)

deriving stock instance (f ~ Result) => Show (Tag f)
deriving stock instance (f ~ Result) => Show (ArticleTag f)

-- | 获取连接池配置
getPoolConfig :: IO Config
getPoolConfig = do
  let dbHost = ""
      dbPort = 5432
      dbUser = "postgres"
      dbPass = ""
      dbName = "yesblog"
      connSettings = Connection.settings dbHost dbPort dbUser dbPass dbName
  pure $
    settings
      [ size 10
      , acquisitionTimeout (secondsToDiffTime 5)
      , agingTimeout (secondsToDiffTime 300)
      , idlenessTimeout (secondsToDiffTime 60)
      , staticConnectionSettings connSettings
      , initSession pass
      , observationHandler (const pass)
      ]

-- 运行查询
runQuery :: (Serializable (a Expr) (a Result)) => Pool.Pool -> Query (a Expr) -> IO (Either Pool.UsageError [a Result])
runQuery pool q = Pool.use pool (statement () (Rel8.run $ select q))

paginateAndSort ::
  (Rel8able a) =>
  -- | 排序规则
  Order (a Expr) ->
  -- | 基础查询
  Query (a Expr) ->
  -- | 当前页码
  Int ->
  -- | 每页大小
  Int ->
  Query (a Expr)
paginateAndSort sortOrder baseQuery page pageSize =
  let offsetVal = (page - 1) * pageSize
   in limit (fromIntegral pageSize)
        . offset (fromIntegral offsetVal)
        . orderBy sortOrder -- 直接使用传入的排序规则
        $ baseQuery

-- | 获取文章列表，支持分页和按最新发布时间排序
getArticlesPaginated :: Int -> Int -> Query (Article Expr)
getArticlesPaginated =
  paginateAndSort (articleCreatedAt >$< desc) $ each articleSchema

-- 通过标签名找Tag
tagForName :: Text -> Query (Tag Expr)
tagForName name_ = do
  tag <- allTags
  where_ (tagName tag ==. lit name_)
  return tag

-- 通过tag找articles
articlesForTag :: Tag Expr -> Query (Article Expr)
articlesForTag tag = do
  article <- each articleSchema
  articleTag <- each articleTagSchema
  where_ $
    (articleId article ==. articleTagArticleId articleTag)
      &&. (articleTagTagId articleTag ==. tagId tag)
  return article

allTags :: Query (Tag Expr)
allTags = do
  tag <- each tagSchema
  where_ $ tagEnable tag ==. lit True
  return tag

-- | 获取文章列表，支持按标签过滤、分页和按最新发布时间排序
getArticlesByTagPaginated :: Text -> Int -> Int -> Query (Article Expr)
getArticlesByTagPaginated tagNameToFilter =
  paginateAndSort (articleCreatedAt >$< desc) $ do
    tag <- tagForName tagNameToFilter
    article <- articlesForTag tag
    where_ (articleEnable article ==. lit True)
    return article

runApp :: IO ()
runApp = do
  poolConfig <- getPoolConfig
  pool <- Pool.acquire poolConfig
  -- 示例：获取第一页，每页2篇文章
  articlesPage1 <- runQuery pool $ getArticlesPaginated 1 2
  putStrLn "\nArticles Page 1 (2 articles per page):"
  case articlesPage1 of
    Left err -> putStrLn $ "Error fetching articles: " ++ show err
    Right arts -> traverse_ print arts

  -- 示例：获取第二页，每页2篇文章
  articlesPage2 <- runQuery pool $ getArticlesPaginated 2 2
  putStrLn "\nArticles Page 2 (2 articles per page):"
  case articlesPage2 of
    Left err -> putStrLn $ "Error fetching articles: " ++ show err
    Right arts -> traverse_ print arts

  -- 示例：获取标签为 "Haskell" 的第一页文章，每页1篇文章
  articlesByTagPage1 <- runQuery pool $ getArticlesByTagPaginated "Haskell" 1 1
  putStrLn "\nArticles with tag 'Haskell' Page 1 (1 article per page):"
  case articlesByTagPage1 of
    Left err -> putStrLn $ "Error fetching articles by tag: " ++ show err
    Right arts -> traverse_ print arts

  -- 示例：获取标签为 "Haskell" 的第二页文章，每页1篇文章
  articlesByTagPage2 <- runQuery pool $ getArticlesByTagPaginated "Haskell" 2 1
  putStrLn "\nArticles with tag 'Haskell' Page 2 (1 article per page):"
  case articlesByTagPage2 of
    Left err -> putStrLn $ "Error fetching articles by tag: " ++ show err
    Right arts -> traverse_ print arts

  -- 释放连接
  Pool.release pool
