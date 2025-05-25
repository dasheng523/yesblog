# Tech Context

## Technologies Used
- [Fill in technologies used here]

## Development Setup
- [Fill in development environment setup here]

## Technical Constraints
- __Haskell__: 核心开发语言。

- __Rel8__: 类型安全的 Haskell SQL 库，用于构建和执行 PostgreSQL 查询。

- __Hasql__: Rel8 底层使用的 PostgreSQL 驱动，负责实际的数据库连接和会话管理。

- __Servant__: Haskell Web API 框架，未来可能用于暴露文章列表 API。

- __数据库查询优化__: 分页（`LIMIT`, `OFFSET`）和排序（`ORDER BY`）是常见的数据库查询优化技术。

- __类型系统__: Haskell 的强类型系统在编译时捕获数据库查询中的类型不匹配错误。

- __模块化编程__: 将数据库操作封装在单独的模块中（如 `Rel8Example.hs`）。

- __记忆库 (Memory Bank)__: 用于持久化项目上下文、技术决策和学习经验的文档系统。

- __Relude Prelude 统一约定__：项目采用 relude 作为 Prelude（见下方 mixins 配置），所有 Prelude 函数（如 `newIORef`、`readIORef`、`modifyIORef'`、`lookupEnv` 等）必须使用 relude 版本，禁止 import `Data.IORef`、`System.Environment` 等标准库模块，否则极易引发类型歧义和编译错误。
- __常见歧义与解决__：如遇到“Ambiguous occurrence”类型错误，优先排查是否有重复 import，统一用 relude Prelude 版本，删除显式 import。
- __团队约定__：所有成员需知晓 relude mixins 配置，遵循统一风格，相关经验已记录于记忆库，后续遇到歧义优先查阅本条。

## Dependencies
- [Fill in project dependencies here]

### Haskell Cabal Mixins Configuration Summary
With `mixins: base hiding (Prelude), relude (Relude as Prelude, ...)` configured in `.cabal`, `Prelude` in your code should refer to `Relude` (from the `relude` package) instead of the default `base` package's `Prelude`. This means your code should utilize `Relude`'s more modern and ergonomic alternative, and modules like `Relude.Container.One` should be directly available.

**Best Practice:** Never import `Data.IORef`, `System.Environment` or other standard Prelude-overlapping modules unless absolutely necessary（如需限定性 import，务必避免与 Prelude 冲突的函数名），所有常用函数直接用 relude Prelude 版本，保持风格统一。

## Tool Usage Patterns
- *(Important)* After modifying `.cabal` or `.nix` files, user confirmation is required before proceeding.
- Memory bank updates should be confined to a single appropriate file; duplicate information across multiple locations is unnecessary.
- After each coding session, `cabal build` must be run for verification.
- **Rel8 Source Lookup**: When encountering issues or questions related to `rel8`, prioritize checking the local source code located at `gitdoc/rel8/src` to avoid hallucinations and get up-to-date information, as the `docs` directory is outdated.
