{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Rel8Example where

import Data.Text
import Data.Time
import GHC.Generics
import Hasql.Connection
import Hasql.Session (QueryError, run, statement)
import Rel8
import Prelude

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

{- | 连接 PostgreSQL 数据库的设置
注意：这里假设 PostgreSQL 运行在本地，且用户名为 'postgres'，无密码。
您可能需要根据实际 PostgreSQL 配置修改这些设置。
-}
connectionSettings :: Settings
connectionSettings = "user=postgres dbname=yesblog"

-- | 获取文章列表，支持分页和按最新发布时间排序
getArticlesPaginated :: Connection -> Int -> Int -> IO (Either Hasql.Session.QueryError [Article Result])
getArticlesPaginated conn page pageSize = do
  let offsetVal = (page - 1) * pageSize
      articleQuery =
        limit (fromIntegral pageSize)
          . offset (fromIntegral offsetVal)
          . orderBy (articleCreatedAt >$< desc) -- 使用 >$< 运算符
          $ each articleSchema
  Hasql.Session.run (Hasql.Session.statement () (Rel8.run $ select articleQuery)) conn

runApp :: IO ()
runApp = do
  Right conn <- acquire connectionSettings
  -- 示例：获取第一页，每页2篇文章
  articlesPage1 <- getArticlesPaginated conn 1 2
  putStrLn "\nArticles Page 1 (2 articles per page):"
  case articlesPage1 of
    Left err -> putStrLn $ "Error fetching articles: " ++ show err
    Right arts -> traverse_ print arts

  -- 示例：获取第二页，每页2篇文章
  articlesPage2 <- getArticlesPaginated conn 2 2
  putStrLn "\nArticles Page 2 (2 articles per page):"
  case articlesPage2 of
    Left err -> putStrLn $ "Error fetching articles: " ++ show err
    Right arts -> traverse_ print arts

  -- 释放连接
  release conn
