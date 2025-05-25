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

import Data.Snowflake
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

-- | 生成新的雪花ID
generateSnowflakeId :: SnowflakeGen -> IO Text
generateSnowflakeId gen = do
  sid <- nextSnowflake gen
  return $ show sid

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

-- | 所有标签
allTags :: Query (Tag Expr)
allTags = do
  tag <- each tagSchema
  where_ $ tagEnable tag ==. lit True
  return tag

-- | 获取标签列表，支持分页和按最新发布时间排序
getTagsPaginated :: Int -> Int -> Query (Tag Expr)
getTagsPaginated =
  paginateAndSort (tagCreatedAt >$< desc) allTags

-- | 获取文章列表，支持按标签过滤、分页和按最新发布时间排序
getArticlesByTagPaginated :: Text -> Int -> Int -> Query (Article Expr)
getArticlesByTagPaginated tagNameToFilter =
  paginateAndSort (articleCreatedAt >$< desc) $ do
    tag <- tagForName tagNameToFilter
    article <- articlesForTag tag
    where_ (articleEnable article ==. lit True)
    return article

createTagWithName :: SnowflakeGen -> Pool.Pool -> Text -> IO (Either Pool.UsageError [Tag Result])
createTagWithName gen pool tagName' = do
  now <- getCurrentTime
  tagIdText <- generateSnowflakeId gen
  let tag =
        Tag
          { tagId = lit (TagId tagIdText)
          , tagName = lit tagName'
          , tagCreatedAt = lit now
          , tagUpdatedAt = lit now
          , tagEnable = lit True
          }
  createTag tag pool

runApp :: IO ()
runApp = do
  poolConfig <- getPoolConfig
  pool <- Pool.acquire poolConfig
  -- 初始化雪花ID生成器
  gen <- newSnowflakeGen defaultConfig 0

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

  -- 示例：新增标签
  putStrLn "\n--- Tag CRUD Examples ---"
  currentTime <- getCurrentTime
  tagId <- generateSnowflakeId gen
  let newTagId = TagId tagId
      newTagName = "Haskell"
      newTag =
        Tag
          { tagId = lit newTagId
          , tagName = lit newTagName
          , tagCreatedAt = lit currentTime
          , tagUpdatedAt = lit currentTime
          , tagEnable = lit True
          }
  putStrLn $ "Creating tag: " ++ show newTagId
  createdTagResult <- createTag newTag pool
  case createdTagResult of
    Left err -> putStrLn $ "Error creating tag: " ++ show err
    Right tags -> putStrLn $ "Created tag: " ++ show tags

  -- 示例：更新标签
  let updatedTagName = "Functional Programming"
  updatedTagResult <- updateTag newTagId (\t -> t {tagName = lit updatedTagName, tagUpdatedAt = lit currentTime}) pool
  case updatedTagResult of
    Left err -> putStrLn $ "Error updating tag: " ++ show err
    Right tags -> putStrLn $ "Updated tag: " ++ show tags

  -- 示例：删除标签
  putStrLn $ "Deleting tag: " ++ show newTagId
  deletedTagResult <- deleteTag newTagId pool
  case deletedTagResult of
    Left err -> putStrLn $ "Error deleting tag: " ++ show err
    Right tags -> putStrLn $ "Deleted tag: " ++ show tags

  -- 示例：新增文章
  putStrLn "\n--- Article CRUD Examples ---"
  currentTime <- getCurrentTime
  articleId <- generateSnowflakeId gen
  let newArticleId = ArticleId articleId
      newArticleName = "My First Article"
      newArticleContent = Just "This is the content of my first article."
      newArticle =
        Article
          { articleId = lit newArticleId
          , articleName = lit newArticleName
          , articleContent = lit newArticleContent
          , articleCreatedAt = lit currentTime
          , articleUpdatedAt = lit currentTime
          , articleEnable = lit True
          }
  putStrLn $ "Creating article: " ++ show newArticleId
  createdArticleResult <- createArticle newArticle pool
  case createdArticleResult of
    Left err -> putStrLn $ "Error creating article: " ++ show err
    Right articles -> putStrLn $ "Created article: " ++ show articles

  -- 示例：更新文章
  let updatedArticleContent = Just "This is the updated content."
  updatedArticleResult <- updateArticle newArticleId (\a -> a {articleContent = lit updatedArticleContent, articleUpdatedAt = lit currentTime}) pool
  case updatedArticleResult of
    Left err -> putStrLn $ "Error updating article: " ++ show err
    Right articles -> putStrLn $ "Updated article: " ++ show articles

  -- 示例：删除文章
  putStrLn $ "Deleting article: " ++ show newArticleId
  deletedArticleResult <- deleteArticle newArticleId pool
  case deletedArticleResult of
    Left err -> putStrLn $ "Error deleting article: " ++ show err
    Right articles -> putStrLn $ "Deleted article: " ++ show articles

-- 通用插入
genericInsert ::
  (Rel8able a, Serializable (a Expr) (a Result)) =>
  TableSchema (a Name) ->
  a Expr ->
  Pool.Pool ->
  IO (Either Pool.UsageError [a Result])
genericInsert schema_ row pool =
  Pool.use
    pool
    ( statement
        ()
        ( Rel8.run $
            insert $
              Insert
                { into = schema_
                , rows = values [row]
                , onConflict = DoNothing
                , returning = Returning id
                }
        )
    )

-- 通用更新
genericUpdate ::
  (Rel8able a, Serializable (a Expr) (a Result)) =>
  TableSchema (a Name) ->
  (a Expr -> Expr Bool) -> -- 过滤条件
  (a Expr -> a Expr) -> -- 更新内容
  Pool.Pool ->
  IO (Either Pool.UsageError [a Result])
genericUpdate schema_ cond updateFn pool =
  Pool.use
    pool
    ( statement
        ()
        ( Rel8.run $
            update $
              Update
                { target = schema_
                , from = each schema_
                , set = \_ row -> updateFn row
                , updateWhere = \_ row -> cond row
                , returning = Returning id
                }
        )
    )

-- 通用删除
genericDelete ::
  (Rel8able a, Serializable (a Expr) (a Result)) =>
  TableSchema (a Name) ->
  -- | 过滤条件
  (a Expr -> Expr Bool) ->
  Pool.Pool ->
  IO (Either Pool.UsageError [a Result])
genericDelete schema_ cond pool =
  Pool.use
    pool
    ( statement
        ()
        ( Rel8.run $
            delete $
              Delete
                { from = schema_
                , using = pass
                , deleteWhere = const cond
                , returning = Returning id
                }
        )
    )

-- | 标签的 CRUD 操作
createTag :: Tag Expr -> Pool.Pool -> IO (Either Pool.UsageError [Tag Result])
createTag = genericInsert tagSchema

updateTag :: TagId -> (Tag Expr -> Tag Expr) -> Pool.Pool -> IO (Either Pool.UsageError [Tag Result])
updateTag tid = genericUpdate tagSchema (\t -> tagId t ==. lit tid)

deleteTag :: TagId -> Pool.Pool -> IO (Either Pool.UsageError [Tag Result])
deleteTag tid = genericDelete tagSchema (\t -> tagId t ==. lit tid)

-- | 文章的 CRUD 操作
createArticle :: Article Expr -> Pool.Pool -> IO (Either Pool.UsageError [Article Result])
createArticle = genericInsert articleSchema

updateArticle :: ArticleId -> (Article Expr -> Article Expr) -> Pool.Pool -> IO (Either Pool.UsageError [Article Result])
updateArticle aid = genericUpdate articleSchema (\a -> articleId a ==. lit aid)

deleteArticle :: ArticleId -> Pool.Pool -> IO (Either Pool.UsageError [Article Result])
deleteArticle aid = genericDelete articleSchema (\a -> articleId a ==. lit aid)
