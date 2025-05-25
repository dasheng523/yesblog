# Progress

## What works
- 所有项目更改已成功提交到仓库。
- 在 `src/Rel8Example.hs` 中实现了 Rel8 查询示例，用于获取和打印文章。
- 成功建立了数据库连接并使用 Rel8 查询了数据。
- 在 `src/Rel8Example.hs` 中实现了 `getArticlesPaginated` 函数，用于分页获取文章列表。
- `Server.hs` 中标签管理页面的局部刷新功能已实现，包括新增标签后标签列表的动态更新和输入框的清空。
- `Server.hs` 中 `Rel8Example` 模块的导入已修正。
- `Server.hs` 中 `renderTagList` 函数的类型签名已修正。
- `Server.hs` 中 `Rel8.Result` 类型已正确导入。

## What's left to build
- 个人博客
  - HTML 页面
    - 首页
    - 标签页
    - 关于页
  - 数据库逻辑
    - 导入数据库结构 (已完成，用户手动执行)
    - 调通数据库连接 (已完成)
  - 后台界面
    - 简单后端登录验证
    - 简单文章增删改查功能
  - 博客样式美化
  - **实现 `getArticlesByTagPaginated` 函数**：根据标签过滤文章并支持分页。

## Current status
- Rel8 查询文章的功能已实现并验证。
- `getArticlesPaginated` 函数已实现，但需要验证其最终状态和导入。
- 个人博客项目正在进行中，主要任务包括前端 HTML 页面开发、后台管理界面开发、博客样式美化以及实现按标签分页获取文章的功能。
- 标签管理页面的局部刷新功能已完成。

## Known issues
- 缺少显式派生策略（`stock`、`anyclass`）导致的警告。
- `PrimaryKey` 的 `Show` 和 `Eq` 实例需要 `UndecidableInstances` 扩展和精确的上下文约束。
- `pre-commit` 钩子在提交时导致问题，已通过清理缓存解决。
- `Rel8Example.hs` 中 `run` 函数名歧义（已通过显式限定解决）。
- `Hasql.Session.run` 参数顺序不正确（已通过查阅 API 文档解决）。
- `relation "article" does not exist` 错误（已通过用户手动执行 `sql/init.sql` 解决）。
