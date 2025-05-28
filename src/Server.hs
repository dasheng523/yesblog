{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Data.Char
import Data.Snowflake
import Data.Typeable
import Hasql.Pool qualified as Pool
import Lucid
import Network.Wai.Handler.Warp qualified as Warp
import Rel8Example
import Servant
import Servant.HTML.Lucid
import StaticFiles
import Web.FormUrlEncoded

data Env = Env
  { envPool :: Pool.Pool
  , envCounter :: IORef Int
  , gen :: SnowflakeGen
  }

linkToText :: (IsElem endpoint API, HasLink endpoint) => Proxy endpoint -> MkLink endpoint Text
linkToText = safeLink' toUrlPiece api

type AppM = ReaderT Env Handler

type TagEndpoint = "tag-manager" :> Get '[HTML] (Html ())
type TagListEndpoint = "tag-manager" :> "list" :> Get '[HTML] (Html ())
type TagCreateEndpoint = "tag-manager" :> "create" :> ReqBody '[FormUrlEncoded] CreateTagForm :> Post '[HTML] (Headers '[Header "HX-Trigger" Text] NoContent)
type TagDeleteEndpoint = "tag-manager" :> Capture "tagId" Text :> Delete '[HTML] (Headers '[Header "HX-Trigger" Text] NoContent)
type TagUpdateEndpoint = "tag-manager" :> Capture "tagId" Text :> ReqBody '[FormUrlEncoded] CreateTagForm :> Put '[HTML] (Headers '[Header "HX-Trigger" Text] NoContent)
type TagEditFormEndpoint = "tag-manager" :> "edit" :> Capture "tagId" Text :> Get '[HTML] (Html ())

type TagManagerAPI =
  TagEndpoint
    :<|> TagListEndpoint
    :<|> TagCreateEndpoint
    :<|> TagDeleteEndpoint
    :<|> TagUpdateEndpoint
    :<|> TagEditFormEndpoint

type CounterCountEndpoint = "counterCount" :> Get '[HTML] (Html ())
type IncrementEndpoint = "increment" :> Post '[HTML] (Headers '[Header "HX-Trigger" Text] NoContent)

type API =
  Get '[HTML] (Html ())
    :<|> "about" :> Get '[HTML] (Html ())
    :<|> "tags" :> Get '[HTML] (Html ())
    :<|> "counter" :> Get '[HTML] (Html ())
    :<|> IncrementEndpoint
    :<|> CounterCountEndpoint
    :<|> TagManagerAPI
    :<|> "article-manager" :> Get '[HTML] (Html ())
    :<|> "static" :> Raw

api :: Proxy API
api = Proxy

server ::
  ServerT API AppM
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
    :<|> endpointHandler (Proxy @TagListEndpoint)
    :<|> createTagR
    :<|> deleteTagR
    :<|> updateTagR
    :<|> editTagFormR

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

pageWithTitle :: Text -> Html () -> Html ()
pageWithTitle = siteLayout

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

class EndpointHandler endpoint where
  endpointHandler :: Proxy endpoint -> AppM (Html ())

-- 2. 为 CounterCountEndpoint 实现实例
instance EndpointHandler CounterCountEndpoint where
  endpointHandler _ = counterCountR

instance EndpointHandler TagListEndpoint where
  endpointHandler _ = renderTagListR

-- 自动事件名生成：类型名转小写加-event
autoEventName :: forall endpoint. (Typeable endpoint) => Proxy endpoint -> Text
autoEventName _ =
  let name = tyConName (typeRepTyCon (typeRep (Proxy :: Proxy endpoint)))
   in toText (map toLower name ++ "-event")

proxyDisplay ::
  ( EndpointHandler endpoint
  , IsElem endpoint API
  , HasLink endpoint
  , MkLink endpoint Text ~ Text
  , Typeable endpoint
  ) =>
  Proxy endpoint ->
  AppM (Html ())
proxyDisplay proxy = do
  html <- endpointHandler proxy
  return $
    div_
      [ data_ "hx-get" (linkToText proxy)
      , data_ "hx-trigger" (autoEventName proxy <> " from:body")
      ]
      html

-- 计数器页面 Handler
counterR :: AppM (Html ())
counterR = do
  countSpan <- proxyDisplay (Proxy @CounterCountEndpoint)
  return $ siteLayout "HTMX Counter Example" $ do
    h1_ "HTMX Counter Example"
    p_ $ do
      "Current count: "
      countSpan
    button_
      [ data_ "hx-post" (linkToText $ Proxy @IncrementEndpoint)
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
  return $ addHeader (autoEventName (Proxy @IncrementEndpoint)) NoContent

-- 渲染标签列表
renderTagListR :: AppM (Html ())
renderTagListR = do
  Env{..} <- ask
  eTags <- liftIO $ runQuery envPool $ getTagsPaginated 1 10
  case eTags of
    Left err ->
      return $ siteLayout "Tag Manager" $ p_ [] $ toHtml (("Error: " <> show err) :: Text)
    Right tags ->
      return $
        table_ [class_ "tag-table min-w-full divide-y divide-gray-200 border border-gray-300 text-sm text-left bg-white"] $ do
          thead_ [class_ "bg-gray-100"] $ tr_ $ do
            th_ [class_ "px-4 py-2 font-semibold text-gray-700"] "ID"
            th_ [class_ "px-4 py-2 font-semibold text-gray-700"] "名称"
            th_ [class_ "px-4 py-2 font-semibold text-gray-700"] "操作"
          tbody_ [class_ "divide-y divide-gray-200"] $
            mconcat
              [ tr_ [class_ "hover:bg-gray-50", id_ $ "tag-row-" <> unTagId (tagId tag)] $ do
                td_ [class_ "px-4 py-2 text-gray-900 whitespace-nowrap"] (toHtml (unTagId $ tagId tag))
                td_ [class_ "px-4 py-2 text-gray-900 whitespace-nowrap"] (toHtml (tagName tag))
                td_ [class_ "px-4 py-2 flex gap-2"] $
                  mconcat
                    [ form_
                        [ data_ "hx-delete" $ linkToText (Proxy @TagDeleteEndpoint) (unTagId $ tagId tag)
                        , data_ "hx-swap" "none"
                        , data_ "hx-confirm" ("确定要删除标签【" <> tagName tag <> "】吗？")
                        ]
                        $ button_ [type_ "submit", class_ "bg-red-500 hover:bg-red-600 text-white px-3 py-1 rounded"] "删除"
                    , button_
                        [ type_ "button"
                        , class_ "bg-yellow-500 hover:bg-yellow-600 text-white px-3 py-1 rounded"
                        , data_ "hx-get" $ "/tag-manager/edit/" <> unTagId (tagId tag)
                        , data_ "hx-target" $ "#tag-row-" <> unTagId (tagId tag)
                        , data_ "hx-swap" "outerHTML"
                        ]
                        "编辑"
                    ]
              | tag <- tags
              ]

-- 新增：编辑表单路由
-- 你需要在 API 类型和 server 里注册 editTagFormR，
-- 例如 type TagEditFormEndpoint = "tag-manager" :> "edit" :> Capture "tagId" Text :> Get '[HTML] (Html ())
-- 并在 TagManagerAPI/ServerT 增加。

-- 标签管理 Handler（简单 CRUD 占位）
tagManagerR :: AppM (Html ())
tagManagerR = do
  tagListDiv <- proxyDisplay (Proxy @TagListEndpoint)
  return $
    siteLayout "Tag Manager" $
      div_ [class_ "space-y-8 max-w-2xl mx-auto p-6 bg-white rounded shadow"] $
        mconcat
          [ h1_ [class_ "text-2xl font-bold text-gray-800 mb-4"] "Tag Manager"
          , div_ [id_ "tag-list-section", class_ "mb-8"] $ do
              p_ [class_ "mb-2 text-gray-600"] "标签列表："
              tagListDiv
          , div_ [id_ "tag-form-section", class_ "bg-gray-50 p-4 rounded border border-gray-200"] $ do
              h2_ [class_ "text-lg font-semibold text-gray-700 mb-2"] "新增标签"
              form_
                [ method_ "post"
                , data_ "hx-post" $ linkToText $ Proxy @TagCreateEndpoint
                , data_ "hx-swap" "none"
                , data_ "hx-on::after-request" "this.reset()"
                , class_ "flex flex-col sm:flex-row items-center gap-2"
                ]
                $ mconcat
                  [ label_ [Lucid.for_ "tag-name-input", class_ "text-gray-700"] "标签名: "
                  , input_ [type_ "text", name_ "name", id_ "tag-name-input", class_ "border border-gray-300 rounded px-3 py-1 focus:outline-none focus:ring-2 focus:ring-blue-200"]
                  , button_ [type_ "submit", class_ "bg-blue-500 hover:bg-blue-600 text-white px-4 py-1 rounded"] "新增"
                  ]
          ]

-- 编辑标签表单渲染
editTagFormR :: Text -> AppM (Html ())
editTagFormR tagIdText = do
  Env{..} <- ask
  eTags <- liftIO $ runQuery envPool (getTagById (TagId tagIdText))
  case eTags of
    Left _ -> return $ tr_ $ td_ [colspan_ "3"] "标签不存在"
    Right [] -> return $ tr_ $ td_ [colspan_ "3"] "标签不存在"
    Right (tag : _) ->
      return $
        tr_ [id_ $ "tag-row-" <> unTagId (tagId tag)] $ do
          td_ [class_ "px-4 py-2"] (toHtml (unTagId $ tagId tag))
          td_ [class_ "px-4 py-2", colspan_ "2"]
            $ form_
              [ method_ "post"
              , data_ "hx-put" $ linkToText (Proxy @TagUpdateEndpoint) (unTagId $ tagId tag)
              , data_ "hx-swap" "none"
              , class_ "flex flex-row items-center gap-2"
              ]
            $ mconcat
              [ input_ [type_ "text", name_ "name", value_ (tagName tag), class_ "border border-gray-300 rounded px-3 py-1"]
              , button_ [type_ "submit", class_ "bg-green-500 hover:bg-green-600 text-white px-3 py-1 rounded"] "保存"
              ]

-- 编辑标签 Handler
updateTagR :: Text -> CreateTagForm -> AppM (Headers '[Header "HX-Trigger" Text] NoContent)
updateTagR tagIdText (CreateTagForm newName) = do
  Env{..} <- ask
  _ <- liftIO $ updateTagName (TagId tagIdText) newName envPool
  return $ addHeader (autoEventName (Proxy @TagListEndpoint)) NoContent

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
createTagR :: CreateTagForm -> AppM (Headers '[Header "HX-Trigger" Text] NoContent)
createTagR (CreateTagForm tagName') = do
  Env{..} <- ask
  _ <- liftIO $ createTagWithName gen envPool tagName'
  return $ addHeader (autoEventName (Proxy @TagListEndpoint)) NoContent

-- 删除标签 Handler
deleteTagR :: Text -> AppM (Headers '[Header "HX-Trigger" Text] NoContent)
deleteTagR tagIdText = do
  Env{..} <- ask
  _ <- liftIO $ deleteTag (TagId tagIdText) envPool
  return $ addHeader (autoEventName (Proxy @TagListEndpoint)) NoContent

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
