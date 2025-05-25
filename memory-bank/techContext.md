# 技术背景

## 使用技术
1. **编程语言**：Haskell (GHC 9.6.6)
2. **ORM框架**：Rel8 (类型安全PostgreSQL操作)
3. **数据库**：PostgreSQL 14+ (支持Snowflake ID扩展)
4. **前端技术**：HTMX (无JavaScript交互)
5. **ID生成**：Twitter Snowflake算法实现
6. **构建工具**：Cabal 3.10.1.0

## 开发环境
1. 构建工具：Cabal (3.10.1.0)
2. 项目配置：`cabal.project` (包含yesblog.cabal)
3. 服务器端口：8000
4. 数据库连接：通过环境变量配置连接池
5. 类型系统：Relude Prelude统一约定（禁止直接导入标准Prelude模块）

## 技术约束
1. 需要PostgreSQL 14+ 支持Snowflake ID
2. HTMX前端需兼容主流浏览器
3. 类型安全要求严格遵循Haskell编译规则
4. 分布式部署需保证ID生成器节点ID唯一性
5. 所有Prelude函数必须使用Relude版本

## 依赖项
```cabal
build-depends:
  base >=4.16 && <4.17,
  rel8 >=0.4.0.0,
  hasql >=2.5.1.0,
  htmx >=0.1.0.0,
  snowflake >=0.1.0.0
```

## Haskell Cabal Mixins 配置摘要
通过 `.cabal` 中配置的 `mixins: base hiding (Prelude), relude (Relude as Prelude, ...)`，项目使用 Relude 的 Prelude 替代标准 Prelude。所有代码中的 Prelude 应该引用 Relude 模块，禁止直接导入 `Data.IORef`、`System.Environment` 等标准库模块，避免类型歧义。

**最佳实践**：
- 所有常用函数直接使用 Relude Prelude 版本
- 遇到"Ambiguous occurrence"类型错误时，优先检查重复导入
- 团队成员需统一遵循 Relude 配置约定
