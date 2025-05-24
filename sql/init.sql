CREATE TABLE article (
  id VARCHAR PRIMARY KEY,
  name TEXT NOT NULL,
  content TEXT,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  enable BOOLEAN NOT NULL DEFAULT TRUE
);

-- 创建 tag 表
CREATE TABLE tag (
  id VARCHAR PRIMARY KEY,
  name TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  enable BOOLEAN NOT NULL DEFAULT TRUE
);

-- 创建 article_tag 关联表
CREATE TABLE article_tag (
  article_id VARCHAR NOT NULL REFERENCES article(id) ON DELETE CASCADE,
  tag_id VARCHAR NOT NULL REFERENCES tag(id) ON DELETE CASCADE,
  PRIMARY KEY (article_id, tag_id)
);

-- 插入测试数据到 article 表
INSERT INTO article (id, name, content) VALUES
('article-1', '我的第一篇博客', '这是我的第一篇博客文章的内容。'),
('article-2', '关于 Haskell 和 Beam', '深入探讨 Haskell 和 Beam 框架的集成。'),
('article-3', 'NixOS 环境配置', '记录在 NixOS 上配置开发环境的经验。');

-- 插入测试数据到 tag 表
INSERT INTO tag (id, name) VALUES
('tag-1', 'Haskell'),
('tag-2', '编程'),
('tag-3', 'NixOS');

-- 插入测试数据到 article_tag 关联表
INSERT INTO article_tag (article_id, tag_id) VALUES
('article-1', 'tag-2'),
('article-2', 'tag-1'),
('article-2', 'tag-2'),
('article-3', 'tag-3');
