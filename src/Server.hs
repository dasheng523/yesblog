{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
import StaticFiles
import Web.FormUrlEncoded (FromForm (..), parseUnique)

-- 全局环境类型，包含数据库连接池和计数器
data Env = Env
  { envPool :: Pool.Pool
  , envCounter :: IORef Int
  , gen :: SnowflakeGen
  }

type AppM = ReaderT Env Handler

type TagManagerAPI =
  Get '[HTML] (Html ()) -- 标签管理页面
    :<|> "create" :> ReqBody '[FormUrlEncoded] CreateTagForm :> Post '[HTML] (Html ())
    :<|> Capture "tagId" Text :> Servant.Delete '[HTML] (Html ())

-- API 类型定义
type API =
  Get '[HTML] (Html ()) -- 根路径，显示主页
    :<|> "about" :> Get '[HTML] (Html ())
    :<|> "tags" :> Get '[HTML] (Html ())
    :<|> "counter" :> Get '[HTML] (Html ()) -- 计数器页面
    :<|> "increment" :> Post '[HTML] (Headers '[Header "HX-Trigger" Text] NoContent) -- 增加计数器
    :<|> "counterCount" :> Get '[HTML] (Html ())
    :<|> "tag-manager" :> TagManagerAPI -- 标签管理页面
    :<|> "article-manager" :> Get '[HTML] (Html ()) -- 文章管理页面
    :<|> "static" :> Raw

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
    :<|> counterCountR
    :<|> tagManagerServer
    :<|> articleManagerR
    :<|> serveDirectoryWebApp "static"

tagManagerServer :: ServerT TagManagerAPI AppM
tagManagerServer =
  tagManagerR
    :<|> createTagR
    :<|> deleteTagR

-- 网站通用布局，包含 HTMX、导航栏等
siteLayout :: Text -> Html () -> Html ()
siteLayout title inner = html_ $ do
  head_ $ do
    title_ (toHtml title)
    link_ [rel_ "stylesheet", href_ $(staticFile "css/output.css")]
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
  Env{..} <- ask
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
  countHtml <- counterCountR
  return $ siteLayout "HTMX Counter Example" $ do
    h1_ "HTMX Counter Example"
    p_ $ do
      "Current count: "
      span_
        [ id_ "counter-display"
        , data_ "hx-get" "/counterCount"
        , data_ "hx-trigger" "my-custom-event from:body"
        ]
        countHtml
    button_
      [ data_ "hx-post" "/increment"
      , data_ "hx-swap" "none"
      ]
      "Increment"
    p_ $ a_ [href_ "/"] "Back to Home"

counterCountR :: AppM (Html ())
counterCountR = do
  Env{..} <- ask
  currentCount <- liftIO $ readIORef envCounter
  return $ toHtml (show currentCount :: Text)

-- 增加计数器 Handler
incrementR :: AppM (Headers '[Header "HX-Trigger" Text] NoContent)
incrementR = do
  Env{..} <- ask
  _ <- liftIO $ modifyIORef envCounter (+ 1)
  _ <- liftIO $ readIORef envCounter
  return $ addHeader "my-custom-event" NoContent

-- 渲染标签列表
renderTagList :: [Tag Result] -> Html ()
renderTagList tags = ul_ $ Prelude.for_ tags $ \tag -> li_ $ do
  toHtml (tagName tag)
  " (ID: "
  toHtml (unTagId $ tagId tag)
  ") "
  toHtml (tagName tag)
  " "
  form_
    [ data_ "hx-delete" ("/tag-manager/" <> unTagId (tagId tag))
    , data_ "hx-target" "#tag-list-section"
    , data_ "hx-swap" "innerHTML"
    , data_ "hx-confirm" ("确定要删除标签【" <> tagName tag <> "】吗？")
    ]
    $ do
      button_ [type_ "submit"] "删除"

-- 标签管理 Handler（简单 CRUD 占位）
tagManagerR :: AppM (Html ())
tagManagerR = do
  Env{..} <- ask
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

newtype CreateIdForm = CreateIdForm {id :: Text}
  deriving stock (Show, Generic)

instance FromForm CreateIdForm where
  fromForm f = CreateIdForm <$> parseUnique "id" f

-- 新增标签 Handler
createTagR :: CreateTagForm -> AppM (Html ())
createTagR (CreateTagForm tagName') = do
  Env{..} <- ask
  _ <- liftIO $ createTagWithName gen envPool tagName'
  eTags <- liftIO $ runQuery envPool $ getTagsPaginated 1 10
  case eTags of
    Left err -> return $ p_ [] $ toHtml (("Error: " <> show err) :: Text)
    Right tags -> do
      return $ div_ [id_ "tag-list-section"] $ do
        p_ "标签列表："
        renderTagList tags

-- 删除标签 Handler
deleteTagR :: Text -> AppM (Html ())
deleteTagR tagIdText = do
  Env{..} <- ask
  _ <- liftIO $ deleteTag (TagId tagIdText) envPool
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
  Env{..} <- ask
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
