# 当前上下文

## 当前工作重点
1. **Snowflake ID集成**：已完成基础ID生成器实现，正在与Rel8 ORM进行集成测试
2. **数据库模式验证**：验证PostgreSQL表结构与Snowflake ID的兼容性
3. **HTMX组件开发**：文章列表和计数器组件已实现基础交互
4. **类型安全验证**：解决Relude Prelude与标准库的模块冲突问题

## 近期变更
1. 成功实现Twitter Snowflake算法的Haskell版本
2. 完成Rel8 ORM与Hasql的连接池配置
3. 修复多处"Ambiguous occurrence"类型错误，统一使用Relude Prelude
4. 前端HTMX组件实现无刷新计数器功能
5. 在 `src/Rel8Example.hs` 中实现 `getArticlesPaginated` 函数，支持按最新发布时间排序和分页
6. 解决了 `Hasql.Session.QueryError` 未导入的问题
7. 解决了 `Rel8.offset` 和 `Rel8.limit` 参数类型问题
8. 解决了 `orderBy` 和 `desc` 运算符的组合问题，通过查阅 Rel8 文档和源码，明确了 `(>$<)` 运算符的正确用法
9. 解决了 `Rel8.run` 函数的参数类型不匹配问题，使用了 `Rel8.run $ select articleQuery`
10. `Server.hs` 中标签管理页面的局部刷新功能已实现，包括新增标签后标签列表的动态更新和输入框的清空。
11. `Server.hs` 中 `Rel8Example` 模块的导入已修正。
12. `Server.hs` 中 `renderTagList` 函数的类型签名已修正。
13. `Server.hs` 中 `Rel8.Result` 类型已正确导入。

## 决策与考量
1. **ID生成策略**：采用时间戳+节点ID+序列号的组合方案，确保分布式唯一性
2. **错误处理机制**：使用EitherT进行统一错误处理，分离业务逻辑与异常处理
3. **模块化设计**：将数据库操作封装在`Database.*`模块中，保持代码结构清晰
4. **Rel8 文档来源**：优先查看 `gitdoc/rel8/src` 目录的源码而非过时的文档
5. **Rel8 查询构建**：明确 `orderBy` 结合 `(>$<)` 运算符的正确用法，以及 `Rel8.run $ select query` 的模式
6. **Relude Prelude 约定**：项目 .cabal 配置了 `mixins: base hiding (Prelude), relude (Relude as Prelude, ...)`，所有 Prelude 函数（如 `newIORef`、`readIORef`、`modifyIORef'`、`lookupEnv` 等）均应使用 relude 版本，禁止 import `Data.IORef`、`System.Environment` 等标准库以避免歧义

## 重要模式
1. **类型驱动开发**：通过Haskell类型系统在编译期捕获潜在错误
2. **配置化节点ID**：通过环境变量配置Snowflake节点ID，支持灵活部署
3. **分层架构**：严格区分Web层、业务层、数据访问层的职责边界
4. **文档查找偏好**: 优先使用 `Context7` (`github.com/upstash/context7-mcp`) 进行通用文档查找
5. **API 查找偏好**: 优先使用 `haskell-hackage-mcp` 进行 Haskell API 特定文档查找
6. 每次操作前，在 <thinking> 标签中包含工具使用原理

## 下一步计划
1. 完成文章CRUD操作与Snowflake ID的绑定
2. 实现 `getArticlesByTagPaginated` 函数，根据标签过滤文章并支持分页
3. 优化数据库连接池配置，提升并发性能
4. 编写单元测试验证ID生成器的唯一性
5. 完善HTMX前端组件的错误提示机制
6. 验证数据库 schema 是否存在以避免“relation does not exist”错误
7. 掌握 Rel8 中 `limit`, `offset`, `orderBy` 和 `desc` 的正确组合方式
