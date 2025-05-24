# Progress

## What works
- 成功读取了 Beam 文档教程。
- 在 `src/BeamExample.hs` 中编写了 Beam Schema 和示例代码。
- 成功修改了 `app/Main.hs` 以调用 Beam 示例。
- 成功更新了 `yesblog.cabal` 以包含 Beam 依赖并暴露 `BeamExample` 模块。
- 解决了 `BeamExample` 模块的编译问题，包括 `hidden module` 和 `deriving strategy` 问题。
- 所有项目更改已成功提交到仓库。
- 在 `src/Rel8Example.hs` 中实现了 Rel8 查询示例，用于获取和打印文章。
- 成功建立了数据库连接并使用 Rel8 查询了数据。
- 在 `src/Rel8Example.hs` 中实现了 `getArticlesPaginated` 函数，用于分页获取文章列表。

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
- Beam 示例已成功集成到项目中并编译。所有更改已提交。
- Rel8 查询文章的功能已实现并验证。
- `getArticlesPaginated` 函数已实现，但需要验证其最终状态和导入。
- 个人博客项目正在进行中，主要任务包括前端 HTML 页面开发、后台管理界面开发、博客样式美化以及实现按标签分页获取文章的功能。

## Known issues
- 初步尝试使用 `beam-migrate` 导致编译错误；根据用户指示，暂时放弃了迁移功能。
- 缺少显式派生策略（`stock`、`anyclass`）导致的警告。
- `PrimaryKey` 的 `Show` 和 `Eq` 实例需要 `UndecidableInstances` 扩展和精确的上下文约束。
- `pre-commit` 钩子在提交时导致问题，已通过清理缓存解决。
- `Rel8Example.hs` 中 `run` 函数名歧义（已通过显式限定解决）。
- `Hasql.Session.run` 参数顺序不正确（已通过查阅 API 文档解决）。
- `relation "article" does not exist` 错误（已通过用户手动执行 `sql/init.sql` 解决）。
- `getArticlesPaginated` 函数在最后一次修改时，由于 `replace_in_file` 的 SEARCH 块不匹配，导致文件回滚，可能需要重新确认其最终状态和导入。
