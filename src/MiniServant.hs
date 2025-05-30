{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- 用于在函数中引用类型变量
{-# LANGUAGE ScopedTypeVariables #-}
-- 确保 TypeApplications 扩展被启用
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module MiniServant where

import GHC.TypeLits (KnownSymbol, Symbol, symbolVal) -- 修正：导入 Symbol
import System.IO.Unsafe (unsafePerformIO)

-- 我们的简化版 Handler Monad
type MyHandlerM = IO

-- 模拟 Servant 的 Get 端点
-- 'path' 是路径段的符号，'a' 是返回类型
data Get (path :: Symbol) a

-- 模拟 Servant 的 Capture 端点
-- 'path' 是路径段的符号，'param' 是捕获参数的类型，'api' 是后续 API
data Capture (path :: Symbol) param api

-- 模拟 Servant 的 NoContent 返回
data NoContent = NoContent

-- 我们的自定义组合器：类型级和值级都用 :##
-- 这是一个中缀数据构造器，也是一个中缀类型构造器
data a :## b = a :## b
infixr 3 :## -- 定义优先级，与 :<|> 类似

-- | HasMyServer 类型类：将 API 类型映射到 Handler 类型，并定义路由逻辑
class HasMyServer api where
  -- | MyServerT 类型族：给定一个 API 类型和 Monad 类型 m，推导出其对应的 Handler 类型
  -- 修正：明确 m 的 kind 约束
  type MyServerT api (m :: Type -> Type) :: Type

  -- | 路由函数：模拟 Servant 的路由机制
  -- 简化版：我们只返回一个字符串，表示哪个 Handler 被调用了
  myRoute :: Proxy api -> MyServerT api MyHandlerM -> Text

-- | Get API 的 HasMyServer 实例
instance (KnownSymbol path, Show a) => HasMyServer (Get path a) where -- 修正：添加 Show a 约束
-- Get path a 对应的 Handler 类型就是返回类型 a 在 MyHandlerM 中的值
  type MyServerT (Get path a) m = m a

  -- 路由逻辑：如果路径匹配，就“调用”Handler
  myRoute Proxy handler =
    let pathStr = symbolVal (Proxy :: Proxy path)
     in "Matched GET /" <> toText pathStr <> ". Handler returned: " <> show (unsafePerformIO handler)

-- | (:##) 组合器的 HasMyServer 实例
instance (HasMyServer a, HasMyServer b) => HasMyServer (a :## b) where
  -- 如果 API 是 a :## b，那么它的 Handler 类型就是 a 的 Handler 类型与 b 的 Handler 类型通过值级 :## 组合
  type MyServerT (a :## b) m = MyServerT a m :## MyServerT b m

  -- 路由逻辑：尝试匹配左边的 API，如果失败（这里简化为“不匹配”），则尝试右边的 API
  myRoute Proxy (handlerA :## handlerB) =
    let
      resultA = myRoute (Proxy :: Proxy a) handlerA
      resultB = myRoute (Proxy :: Proxy b) handlerB
     in
      "Trying first API: "
        <> resultA
        <> "\n"
        <> "Then trying second API: "
        <> resultB

-- | Capture API 的 HasMyServer 实例
instance (KnownSymbol path, HasMyServer api, param ~ Text) => HasMyServer (Capture path param api) where -- 修正：添加 param ~ Text 约束
-- Capture path param api 对应的 Handler 类型是 param -> 后续 api 的 Handler 类型
  type MyServerT (Capture path param api) m = param -> MyServerT api m

  -- 路由逻辑：模拟捕获参数并传递给后续 Handler
  myRoute Proxy handler =
    let pathStr = symbolVal (Proxy :: Proxy path)
        capturedParam :: Text
        capturedParam = "test-param" -- 模拟捕获一个参数，现在 param 强制为 Text
        remainingRoute = myRoute (Proxy :: Proxy api) (handler capturedParam) -- 修正：handler 现在明确是函数
     in "Matched Capture /" <> toText pathStr <> "/<" <> capturedParam <> ">. Remaining route: " <> remainingRoute

-- 定义一个简单的 API
type MySimpleAPI = Get "hello" Text :## Get "world" Int

-- 定义一个包含 Capture 的 API
type MyComplexAPI = Capture "user" Text (Get "profile" Text) :## Get "status" Text

-- 为 MySimpleAPI 提供 Handler 实现
mySimpleServer :: MyServerT MySimpleAPI MyHandlerM
mySimpleServer =
  pure "Hello from /hello!" :## pure 42

-- 为 MyComplexAPI 提供 Handler 实现
myComplexServer :: MyServerT MyComplexAPI MyHandlerM
myComplexServer =
  (\user -> pure ("User profile for " <> user)) :## pure "Server is running."

-- 运行我们的路由
runMiniServant :: (HasMyServer api) => Proxy api -> MyServerT api MyHandlerM -> IO ()
runMiniServant p server = do
  putStrLn $ "Routing for API: " ++ show p -- 修正：使用 ++ 和 show
  putStrLn $ toString $ myRoute p server
  putStrLn "---"

main :: IO ()
main = do
  runMiniServant (Proxy @MySimpleAPI) mySimpleServer
  runMiniServant (Proxy @MyComplexAPI) myComplexServer
