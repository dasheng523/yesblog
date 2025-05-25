{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Data.Snowflake
import Hasql.Pool qualified as Pool
import Lucid
import Lucid.Servant
import Network.Wai.Handler.Warp qualified as Warp
import Rel8
import Rel8Example
import Servant
import Servant.HTML.Lucid
import Web.FormUrlEncoded (FromForm (..), parseUnique)

-- 全局环境类型，包含数据库连接池和计数器
data Env = Env
  { envPool :: Pool.Pool
  , envCounter :: IORef Int
  , gen :: SnowflakeGen
  }

type AppM = ReaderT Env Handler

-- API 类型定义
type API =
  Get '[HTML] (Html ()) -- 根路径，显示主页
    :<|> "about" :> Get '[HTML] (Html ())
    :<|> "tags" :> Get '[HTML] (Html ())
    :<|> "counter" :> Get '[HTML] (Html ()) -- 计数器页面
    :<|> "increment" :> Post '[HTML] (Html ()) -- 增加计数器
    :<|> "tag-manager" :> Get '[HTML] (Html ()) -- 标签管理页面
    :<|> "tag-manager" :> "create" :> ReqBody '[FormUrlEncoded] CreateTagForm :> Post '[HTML] (Html ())
    :<|> "article-manager" :> Get '[HTML] (Html ()) -- 文章管理页面

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
    :<|> tagManagerR
    :<|> createTagR
    :<|> articleManagerR

-- 网站通用布局，包含 HTMX、导航栏等
siteLayout :: Text -> Html () -> Html ()
siteLayout title inner = html_ $ do
  head_ $ do
    title_ (toHtml title)
    script_ [src_ "https://unpkg.com/htmx.org@1.9.10"] ("" :: Html ())
  body_ $ do
    nav_ [class_ "navbar"] $ do
      a_ [href_ "/"] "Home"
      span_ " | "
      a_ [href_ "/about"] "About"
      span_ " | "
      a_ [href_ "/tags"] "Tags"
      span_ " | "
      a_ [href_ "/counter"] "Counter"
      span_ " | "
      a_ [href_ "/tag-manager"] "Tag Manager"
      span_ " | "
      a_ [href_ "/article-manager"] "Article Manager"
    div_ [class_ "main-content"] inner
    footer_ [class_ "footer"] "© 2025 YesBlog"

-- pageWithTitle 兼容层，推荐直接用 siteLayout
pageWithTitle :: Text -> Html () -> Html ()
pageWithTitle = siteLayout

-- 首页 Handler，复用全局 Pool
homeR :: AppM (Html ())
homeR = do
  Env {..} <- ask
  eTags <- liftIO $ runQuery envPool allTags
  case eTags of
    Left err -> do
      liftIO $ putStrLn $ "Error fetching tags: " ++ show err
      return $ siteLayout "Home" $ p_ [class_ "content"] $ do
        b_ [] "Home"
        p_ [] "Error: Could not fetch tags."
    Right tags ->
      return $ siteLayout "Home" $ p_ [class_ "content"] $ do
        b_ [] "Home"
        p_ [] $
          a_
            [ href_ "/counter"
            , data_ "hx-get" "/counter"
            , data_ "hx-target" "body"
            , data_ "hx-push-url" "true"
            ]
            "Go to Counter Example"
        h2_ "Available Tags:"
        ul_ $ traverse_ (\tag -> li_ (toHtml (tagName tag :: Text))) tags

aboutR :: AppM (Html ())
aboutR = return $
  siteLayout "About" $
    p_ [class_ "content"] $ do
      b_ [] "about"

tagsR :: AppM (Html ())
tagsR = return $
  siteLayout "Tags" $
    p_ [class_ "content"] $ do
      b_ [] "tags"

-- 计数器页面 Handler
counterR :: AppM (Html ())
counterR = do
  Env {..} <- ask
  currentCount <- liftIO $ readIORef envCounter
  return $ siteLayout "HTMX Counter Example" $ do
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
  _ <- liftIO $ modifyIORef envCounter (+ 1)
  newCount <- liftIO $ readIORef envCounter
  return $ span_ [id_ "counter-display"] $ toHtml (show (newCount :: Int) :: Text)

-- 渲染标签列表
renderTagList :: [Tag Result] -> Html ()
renderTagList tags = ul_ $ Prelude.for_ tags $ \tag -> li_ $ do
  toHtml (tagName tag)
  " "
  form_ [method_ "post", action_ "/tag-manager/delete"] $ do
    input_ [type_ "hidden", name_ "id", value_ (show $ tagId tag)]
    button_ [type_ "submit"] "删除"

-- 标签管理 Handler（简单 CRUD 占位）
tagManagerR :: AppM (Html ())
tagManagerR = do
  Env {..} <- ask
  eTags <- liftIO $ runQuery envPool $ getTagsPaginated 1 10
  case eTags of
    Left err ->
      return $ siteLayout "Tag Manager" $ p_ [] $ toHtml (("Error: " <> show err) :: Text)
    Right tags ->
      return $
        siteLayout "Tag Manager" $
          mconcat
            [ h1_ "Tag Manager"
            , div_ [id_ "tag-list-section"] $ do
                -- 封装标签列表
                p_ "标签列表："
                renderTagList tags
            , div_ [id_ "tag-form-section"] $ do
                -- 封装新增标签表单
                h2_ "新增标签"
                form_
                  [ method_ "post"
                  , data_ "hx-post" "/tag-manager/create"
                  , data_ "hx-target" "#tag-list-section"
                  , data_ "hx-swap" "innerHTML"
                  , data_ "hx-on::after-request" "this.reset()"
                  ]
                  $ mconcat
                    [ label_ "标签名: "
                    , input_ [type_ "text", name_ "name", id_ "tag-name-input"]
                    , button_ [type_ "submit"] "新增"
                    ]
            ]

-- 新增标签表单类型
newtype CreateTagForm = CreateTagForm {name :: Text}
  deriving stock (Show, Generic)

instance FromForm CreateTagForm where
  fromForm f = CreateTagForm <$> parseUnique "name" f

-- 新增标签 Handler
createTagR :: CreateTagForm -> AppM (Html ())
createTagR (CreateTagForm tagName') = do
  Env {..} <- ask
  _ <- liftIO $ createTagWithName gen envPool tagName'
  eTags <- liftIO $ runQuery envPool $ getTagsPaginated 1 10
  case eTags of
    Left err -> return $ p_ [] $ toHtml (("Error: " <> show err) :: Text)
    Right tags -> do
      return $ div_ [id_ "tag-list-section"] $ do
        p_ "标签列表："
        renderTagList tags

-- 文章管理 Handler（简单 CRUD 占位）
articleManagerR :: AppM (Html ())
articleManagerR = do
  Env {..} <- ask
  eArticles <- liftIO $ runQuery envPool (getArticlesPaginated 1 10)
  case eArticles of
    Left err ->
      return $ siteLayout "Article Manager" $ p_ [] $ toHtml (("Error: " <> show err) :: Text)
    Right articles ->
      return $
        siteLayout "Article Manager" $
          mconcat
            [ h1_ "Article Manager"
            , p_ "文章列表："
            , ul_ $
                mconcat $
                  map
                    ( \art ->
                        li_ $
                          mconcat
                            [ toHtml (articleName art)
                            , " "
                            , form_ [method_ "post", action_ "/article-manager/delete"] $
                                mconcat
                                  [ input_ [type_ "hidden", name_ "id", value_ (show $ articleId art)]
                                  , button_ [type_ "submit"] "删除"
                                  ]
                            ]
                    )
                    articles
            , h2_ "新增文章"
            , form_ [method_ "post", action_ "/article-manager/create"] $
                mconcat
                  [ label_ "标题: "
                  , input_ [type_ "text", name_ "name"]
                  , label_ "内容: "
                  , textarea_ [name_ "content"] ""
                  , button_ [type_ "submit"] "新增"
                  ]
            ]

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
  gen <- newSnowflakeGen defaultConfig 0
  let env = Env pool counterVar gen
  putStrLn $ "http://localhost:" ++ show port ++ "/"
  putStrLn $ "HTMX Counter Example at http://localhost:" ++ show port ++ "/counter"
  Warp.run port (app env)
