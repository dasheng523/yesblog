{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude.Compat
import Prelude ()

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Parser qualified
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)

import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time (UTCTime)
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import System.Directory
import Text.Blaze
import Text.Blaze.Html qualified
import Text.Blaze.Html.Renderer.Utf8

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  }
  deriving stock (Eq, Show, Generic)

type UserAPI1 = "users" :> Get '[JSON] [User]

type UserAPI2 =
  "users" :> Get '[JSON] [User]
    :<|> "albert" :> Get '[JSON] User
    :<|> "isaac" :> Get '[JSON] User

instance ToJSON User

users1 :: [User]
users1 =
  [ User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)
  , User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)
  ]

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users2 :: [User]
users2 = [isaac, albert]

server1 :: Server UserAPI1
server1 = return users1

server2 :: Server UserAPI2
server2 =
  return users2
    :<|> return albert
    :<|> return isaac

userAPI :: Proxy UserAPI1
userAPI = Proxy

userAPI2 :: Proxy UserAPI2
userAPI2 = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI server1

app2 :: Application
app2 = serve userAPI2 server2

type API =
  "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
    :<|> "hello" :> QueryParam' '[Required] "name" String :> Get '[JSON] HelloMessage
    :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  }
  deriving stock (Generic)

instance ToJSON Position

newtype HelloMessage = HelloMessage {msg :: String}
  deriving stock (Generic)

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  }
  deriving stock (Generic)

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  }
  deriving stock (Generic)

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where
    from' = "great@company.com"
    to' = clientEmail c
    subject' = "Hey " ++ clientName c ++ ", we miss you!"
    body' =
      "Hi "
        ++ clientName c
        ++ ",\n\n"
        ++ "Since you've recently turned "
        ++ show (clientAge c)
        ++ ", have you checked out our latest "
        ++ intercalate ", " (clientInterestedIn c)
        ++ " products? Give us a visit!"

server3 :: Server API
server3 =
  position
    :<|> hello
    :<|> marketing
  where
    position :: Int -> Int -> Handler Position
    position x y = return (Position x y)

    hello :: String -> Handler HelloMessage
    hello mname = return . HelloMessage $ "Hello, " ++ mname

    marketing :: ClientInfo -> Handler Email
    marketing clientinfo = return (emailForClient clientinfo)

userAPI3 :: Proxy API
userAPI3 = Proxy

app3 :: Application
app3 = serve userAPI3 server3

data Person = Person
  { firstName :: String
  , lastName :: String
  }
  deriving (Generic) -- for the JSON instance

instance ToJSON Person

type PersonAPI = "persons" :> Get '[JSON, HTML] [Person]

-- HTML serialization of a single person
instance ToHtml Person where
  toHtml person =
    tr_ $ do
      td_ (toHtml $ firstName person)
      td_ (toHtml $ lastName person)

  -- do not worry too much about this
  toHtmlRaw = toHtml

-- HTML serialization of a list of persons
instance ToHtml [Person] where
  toHtml persons = table_ $ do
    tr_ $ do
      th_ "first name"
      th_ "last name"

    -- this just calls toHtml on each person of the list
    -- and concatenates the resulting pieces of HTML together
    foldMap toHtml persons

  toHtmlRaw = toHtml

people :: [Person]
people =
  [ Person "Isaac" "Newton"
  , Person "Albert" "Einstein"
  ]

personAPI :: Proxy PersonAPI
personAPI = Proxy

server4 :: Server PersonAPI
server4 = return people

app4 :: Application
app4 = serve personAPI server4
