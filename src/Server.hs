{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Hasql.Pool qualified as Pool
import Lucid
import Lucid.Servant
import Network.Wai.Handler.Warp qualified as Warp
import Rel8Example (Tag (tagName), allTags, getPoolConfig, runQuery)
import Servant
import Servant.HTML.Lucid

-- 全局环境类型，包含数据库连接池和计数器
data Env = Env
  { envPool :: Pool.Pool
  , envCounter :: IORef Int
  }

type AppM = ReaderT Env Handler

-- API 类型定义
type API =
  Get '[HTML] (Html ()) -- 根路径，显示主页
    :<|> "about" :> Get '[HTML] (Html ())
    :<|> "tags" :> Get '[HTML] (Html ())
    :<|> "counter" :> Get '[HTML] (Html ()) -- 计数器页面
    :<|> "increment" :> Post '[HTML] (Html ()) -- 增加计数器

api :: Proxy API
api = Proxy

apiLink_ ::
  (IsElem endpoint API, HasLink endpoint) =>
  Proxy endpoint ->
  MkLink endpoint Attribute
apiLink_ = safeAbsHref_ (Proxy :: Proxy API)

-- ServerT 版本，所有 Handler 都在 ReaderT Env Handler monad 中
server :: ServerT API AppM
server =
  homeR
    :<|> aboutR
    :<|> tagsR
    :<|> counterR
    :<|> incrementR

-- 首页 Handler，复用全局 Pool
homeR :: AppM (Html ())
homeR = do
  Env {..} <- ask
  eTags <- liftIO $ runQuery envPool allTags
  case eTags of
    Left err -> do
      liftIO $ putStrLn $ "Error fetching tags: " ++ show err
      return $ p_ [class_ "content"] $ do
        b_ [] "Home"
        p_ [] "Error: Could not fetch tags."
    Right tags ->
      return $
        p_ [class_ "content"] $ do
          b_ [] "Home"
          p_ [] $ a_ [href_ "/counter"] "Go to Counter Example"
          h2_ "Available Tags:"
          ul_ $ traverse_ (\tag -> li_ (toHtml (tagName tag :: Text))) tags

aboutR :: AppM (Html ())
aboutR = return $
  p_ [class_ "content"] $ do
    b_ [] "about"

tagsR :: AppM (Html ())
tagsR = return $
  p_ [class_ "content"] $ do
    b_ [] "tags"

-- 计数器页面 Handler
counterR :: AppM (Html ())
counterR = do
  Env {..} <- ask
  currentCount <- liftIO $ readIORef envCounter
  return $ do
    html_ $ do
      head_ $ do
        title_ "HTMX Counter Example"
        script_ [src_ "https://unpkg.com/htmx.org@1.9.10"] ("" :: Html ())
      body_ $ do
        h1_ "HTMX Counter Example"
        p_ $ do
          "Current count: "
          span_ [id_ "counter-display"] (toHtml (show currentCount :: Text))
        button_
          [ data_ "hx-post" "/increment"
          , data_ "hx-target" "#counter-display"
          , data_ "hx-swap" "outerHTML"
          ]
          "Increment"
        p_ $ a_ [href_ "/"] "Back to Home"

-- 增加计数器 Handler
incrementR :: AppM (Html ())
incrementR = do
  Env {..} <- ask
  liftIO $ modifyIORef' envCounter (+ 1)
  newCount <- liftIO $ readIORef envCounter
  return $ span_ [id_ "counter-display"] (toHtml (show newCount :: Text))

-- hoistServer 提升 AppM 到 Handler
app :: Env -> Application
app env = serve api $ hoistServer api (nt env) server
  where
    nt :: Env -> AppM a -> Handler a
    nt env' x = runReaderT x env'

-- 应用启动，acquire Pool/init Env/传递给 app
runApp :: IO ()
runApp = do
  port <- fmap (fromMaybe 8000 . (>>= readMaybe)) (lookupEnv "PORT")
  poolConfig <- getPoolConfig
  pool <- Pool.acquire poolConfig
  counterVar <- newIORef 0
  let env = Env pool counterVar
  putStrLn $ "http://localhost:" ++ show port ++ "/"
  putStrLn $ "HTMX Counter Example at http://localhost:" ++ show port ++ "/counter"
  Warp.run port (app env)
