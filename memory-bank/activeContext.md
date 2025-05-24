# Active Context

## Current work focus
- 实现了 `src/Rel8Example.hs` 中的 `getArticlesPaginated` 函数，用于分页获取文章列表。

## Recent changes
- 尝试在 `src/Rel8Example.hs` 中实现 `getArticlesPaginated` 函数，该函数支持按最新发布时间排序和分页。
- 解决了 `Hasql.Session.QueryError` 未导入的问题。
- 解决了 `Rel8.offset` 和 `Rel8.limit` 参数类型问题。
- 解决了 `orderBy` 和 `desc` 运算符的组合问题，通过查阅 Rel8 文档和源码，明确了 `(>$<)` 运算符的正确用法。
- 解决了 `Rel8.run` 函数的参数类型不匹配问题，根据用户提示，使用了 `Rel8.run $ select articleQuery`。
- 更新了记忆库中关于 Rel8 文档查找的偏好，明确了应优先查看 `gitdoc/rel8/src` 目录的源码。

## Next steps
- 验证 `src/Rel8Example.hs` 中 `getArticlesPaginated` 函数的最终状态，确保其导入和实现都已正确。
- 实现 `getArticlesByTagPaginated` 函数，该函数将根据标签过滤文章并支持分页。

## Active decisions and considerations
- 强调清晰精确的工具参数使用。
- 强调逐步验证工具执行结果的重要性。
- 维护全面的记忆库文档以供未来会话使用。
- 澄清了 Haskell 中 `deriving` 策略的重要性，并学习了如何正确使用 `stock` 和 `anyclass`。
- 通过清理缓存和重新提交解决了 `pre-commit` 钩子问题。
- 遇到类型错误时，优先查阅 API 文档（`haskell-hackage-mcp`），从而更深入地理解库函数签名和正确用法。
- **Rel8 文档来源**: 了解到 `gitdoc/rel8/docs` 已过时；未来查找 `rel8` 文档应优先查看 `gitdoc/rel8/src` 以直接检查源码。
- **Rel8 查询构建**: 明确了 `orderBy` 结合 `(>$<)` 运算符的正确用法，以及 `Rel8.run $ select query` 的模式。

## Important patterns and preferences
- 优先使用专门的 MCP 工具来完成特定任务。
- **文档查找偏好**: 优先使用 `Context7` (`github.com/upstash/context7-mcp`) 进行通用文档查找。
- **API 查找偏好**: 优先使用 `haskell-hackage-mcp` 进行 Haskell API 特定文档查找。
- **Rel8 源码查找**: 遇到与 `rel8` 相关的问题或疑问时，优先检查本地 `gitdoc/rel8/src` 目录下的源码，以避免幻觉并获取最新信息。
- 严格遵守工具使用格式（XML 标签用于参数）。

## Learnings and project insights
- `.cabal` 文件中 `exposed-modules` 的配置对于模块可见性至关重要。
- `pre-commit` 钩子有时会出现缓存问题，可以通过清理缓存来解决。
- 深入理解 `Hasql.Session.run` 和 `Hasql.Session.statement` 函数签名和参数顺序对于使用 Rel8 进行正确的数据库交互至关重要。
- 验证数据库 schema 是否存在的重要性，以避免“relation does not exist”错误。
- 掌握了 Rel8 中 `limit`, `offset`, `orderBy` 和 `desc` 的正确组合方式，特别是 `(>$<)` 运算符的使用。
- 理解了 `Rel8.run` 和 `select` 函数在执行 Rel8 查询中的协同作用。
