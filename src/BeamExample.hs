{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BeamExample where

import Data.Time (UTCTime, getCurrentTime)
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple (close, execute_, open)

-- Beam Schema 定义将在这里

-- Article Table
data ArticleT f = Article
  { _articleId :: Columnar f Text
  , _articleName :: Columnar f Text
  , _articleContent :: Columnar f (Maybe Text)
  , _articleCreatedAt :: Columnar f UTCTime
  , _articleUpdatedAt :: Columnar f UTCTime
  , _articleEnable :: Columnar f Bool
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table ArticleT where
  data PrimaryKey ArticleT f = ArticleId (Columnar f Text)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = ArticleId . _articleId

type Article = ArticleT Identity
type ArticleId = PrimaryKey ArticleT Identity

deriving stock instance Show Article
deriving stock instance Eq Article
deriving stock instance Show ArticleId
deriving stock instance Eq ArticleId

-- Tag Table
data TagT f = Tag
  { _tagId :: Columnar f Text
  , _tagName :: Columnar f Text
  , _tagCreatedAt :: Columnar f UTCTime
  , _tagUpdatedAt :: Columnar f UTCTime
  , _tagEnable :: Columnar f Bool
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table TagT where
  data PrimaryKey TagT f = TagId (Columnar f Text)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = TagId . _tagId

type Tag = TagT Identity
type TagId = PrimaryKey TagT Identity

deriving stock instance Show Tag
deriving stock instance Eq Tag
deriving stock instance Show TagId
deriving stock instance Eq TagId

-- ArticleTag Table (Join Table)
data ArticleTagT f = ArticleTag
  { _articleTagArticleId :: PrimaryKey ArticleT f
  , _articleTagTagId :: PrimaryKey TagT f
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table ArticleTagT where
  data PrimaryKey ArticleTagT f = ArticleTagId (PrimaryKey ArticleT f) (PrimaryKey TagT f)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = ArticleTagId <$> _articleTagArticleId <*> _articleTagTagId

type ArticleTag = ArticleTagT Identity
type ArticleTagId = PrimaryKey ArticleTagT Identity

deriving stock instance Show ArticleTag
deriving stock instance Eq ArticleTag
deriving stock instance Show ArticleTagId
deriving stock instance Eq ArticleTagId

-- Database Definition
data BlogDb f = BlogDb
  { _blogDbArticles :: f (TableEntity ArticleT)
  , _blogDbTags :: f (TableEntity TagT)
  , _blogDbArticleTags :: f (TableEntity ArticleTagT)
  }
  deriving stock (Generic)
  deriving anyclass (Database be)

blogDb :: DatabaseSettings be BlogDb
blogDb = defaultDbSettings

-- Beam 示例代码将在这里

runBeamDemo :: IO ()
runBeamDemo = do
  putStrLn "Running Beam Demo..."

  -- Connect to SQLite database
  conn <- open "blog.db"

  -- Query all articles
  liftIO $ putStrLn "\nAll Articles:"
  articles <- runBeamSqlite conn $ runSelectReturningList $ select $ all_ (_blogDbArticles blogDb)
  mapM_ (liftIO . print) articles

  -- Query articles by name
  liftIO $ putStrLn "\nArticles with name 'First Article':"
  firstArticle <- runBeamSqlite conn $ runSelectReturningOne $ select $ do
    article <- all_ (_blogDbArticles blogDb)
    guard_ (_articleName article ==. "First Article")
    return article
  liftIO $ print firstArticle

  -- Update an article
  liftIO $ putStrLn "\nUpdating 'First Article' content..."
  runBeamSqlite conn $
    runUpdate $
      update
        (_blogDbArticles blogDb)
        (\article -> _articleContent article <-. just_ "Updated content for the first article.")
        (\article -> _articleId article ==. "article-1")
  liftIO $ putStrLn "Article updated."

  -- Verify update
  liftIO $ putStrLn "\n'First Article' after update:"
  updatedArticle <- runBeamSqlite conn $ runSelectReturningOne $ select $ do
    article <- all_ (_blogDbArticles blogDb)
    guard_ (_articleId article ==. "article-1")
    return article
  liftIO $ print updatedArticle

  -- Delete an article
  liftIO $ putStrLn "\nDeleting 'Second Article'..."
  runBeamSqlite conn $
    runDelete $
      delete
        (_blogDbArticles blogDb)
        (\article -> _articleId article ==. "article-2")
  liftIO $ putStrLn "Second Article deleted."

  -- Verify deletion
  liftIO $ putStrLn "\nAll Articles after deletion:"
  remainingArticles <- runBeamSqlite conn $ runSelectReturningList $ select $ all_ (_blogDbArticles blogDb)
  mapM_ (liftIO . print) remainingArticles

  -- Close connection
  close conn
  liftIO $ putStrLn "\nBeam Demo Finished."
