-- ConstraintKinds needed only for 7.8.4
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Lucid
import Lucid.Servant

import Servant
import Servant.HTML.Lucid

import Network.Wai.Handler.Warp qualified as Warp

type API =
  Get '[HTML] (Html ())
    :<|> "about" :> Get '[HTML] (Html ())
    :<|> "tags" :> Get '[HTML] (Html ())

api :: Proxy API
api = Proxy

apiLink_ ::
  (IsElem endpoint API, HasLink endpoint) =>
  Proxy endpoint ->
  MkLink endpoint Attribute
apiLink_ = safeAbsHref_ (Proxy :: Proxy API)

server :: Server API
server =
  homeR
    :<|> aboutR
    :<|> tagsR

homeR :: Handler (Html ())
homeR = return $
  p_ [class_ "content"] $ do
    b_ [] "home"

aboutR :: Handler (Html ())
aboutR = return $
  p_ [class_ "content"] $ do
    b_ [] "about"

tagsR :: Handler (Html ())
tagsR = return $
  p_ [class_ "content"] $ do
    b_ [] "tags"

app :: Application
app = serve api server

runApp :: IO ()
runApp = do
  port <- fmap (fromMaybe 8000 . (>>= readMaybe)) (lookupEnv "PORT")
  putStrLn $ "http://localhost:" ++ show port ++ "/"
  Warp.run port app
