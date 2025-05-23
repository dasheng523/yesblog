{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Rel8Test where

import Data.Time
import Main.Utf8 qualified as Utf8
import Rel8 hiding (run)

newtype ArticleId = ArticleId {toInt64 :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show)

data Article f = Article
  { id :: Column f ArticleId
  , name :: Column f Text
  , content :: Column f (Maybe Text)
  , createdAt :: Column f UTCTime
  , updatedAt :: Column f UTCTime
  , enable :: Column f Bool
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

newtype TagId = TagId {toInt64 :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show)

data Tag f = Tag
  { id :: Column f TagId
  , name :: Column f Text
  , createdAt :: Column f UTCTime
  , updatedAt :: Column f UTCTime
  , enable :: Column f Bool
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

data ArticleTag f = ArticleTag
  { articleId :: Column f ArticleId
  , tagId :: Column f TagId
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (Article f)
deriving stock instance (f ~ Result) => Show (Tag f)

articleSchema :: TableSchema (Article Name)
articleSchema =
  TableSchema
    { name = "article"
    , columns =
        Article
          { id = "id"
          , name = "name"
          , content = "content"
          , createdAt = "created_at"
          , updatedAt = "updated_at"
          , enable = "enable"
          }
    }

tagSchema :: TableSchema (Tag Name)
tagSchema =
  TableSchema
    { name = "tag"
    , columns =
        Tag
          { id = "id"
          , name = "name"
          , createdAt = "created_at"
          , updatedAt = "updated_at"
          , enable = "enable"
          }
    }

articleTagSchema :: TableSchema (ArticleTag Name)
articleTagSchema =
  TableSchema
    { name = "article_tag"
    , columns =
        ArticleTag
          { articleId = "article_id"
          , tagId = "tag_id"
          }
    }
