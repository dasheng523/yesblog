{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
-- For IORef
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Lucid
import Lucid.Servant

import Servant
import Servant.HTML.Lucid

import Hasql.Pool qualified as Pool
import Network.Wai.Handler.Warp qualified as Warp
import Rel8Example (Tag (tagName), allTags, getPoolConfig, runQuery)

-- 定义新的 API 类型
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

-- 修改 server 函数以包含新的处理函数
server :: IORef Int -> Server API
server counterVar =
  homeR
    :<|> aboutR
    :<|> tagsR
    :<|> counterR counterVar
    :<|> incrementR counterVar

homeR :: Handler (Html ())
homeR = liftIO $ do
  poolConfig <- getPoolConfig
  pool <- Pool.acquire poolConfig
  eTags <- runQuery pool allTags
  _ <- Pool.release pool
  case eTags of
    Left err -> do
      putStrLn $ "Error fetching tags: " ++ show err
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

aboutR :: Handler (Html ())
aboutR = return $
  p_ [class_ "content"] $ do
    b_ [] "about"

tagsR :: Handler (Html ())
tagsR = return $
  p_ [class_ "content"] $ do
    b_ [] "tags"

-- 计数器页面处理函数
counterR :: IORef Int -> Handler (Html ())
counterR counterVar = liftIO $ do
  currentCount <- readIORef counterVar
  return $ do
    html_ $ do
      head_ $ do
        title_ "HTMX Counter Example"
        -- 引入 htmx.min.js
        script_ [src_ "https://unpkg.com/htmx.org@1.9.10"] (pass :: Html ())
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

-- 增加计数器处理函数
incrementR :: IORef Int -> Handler (Html ())
incrementR counterVar = liftIO $ do
  modifyIORef' counterVar (+ 1)
  newCount <- readIORef counterVar
  return $ span_ [id_ "counter-display"] (toHtml (show newCount :: Text))

app :: IORef Int -> Application
app counterVar = serve api (server counterVar)

runApp :: IO ()
runApp = do
  port <- fmap (fromMaybe 8000 . (>>= readMaybe)) (lookupEnv "PORT")
  initialCount <- newIORef 0 -- 初始化计数器
  putStrLn $ "http://localhost:" ++ show port ++ "/"
  putStrLn $ "HTMX Counter Example at http://localhost:" ++ show port ++ "/counter"
  Warp.run port (app initialCount)
