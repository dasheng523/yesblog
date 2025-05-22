{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import Data.Time (UTCTime)
import Servant.API

type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

data SortBy = Age | Name

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: UTCTime
  }

type UserAPI2 =
  "users" :> "list-all" :> Get '[JSON] [User]
    :<|> "list-all" :> "users" :> Get '[JSON] [User]

type UserAPI5 =
  "user" :> Capture "userid" Integer :> Get '[JSON] User
    -- equivalent to 'GET /user/:userid'
    -- except that we explicitly say that "userid"
    -- must be an integer

    :<|> "user" :> Capture "userid" Integer :> DeleteNoContent

-- equivalent to 'DELETE /user/:userid'

type UserAPI10 =
  "users"
    :> Get
        '[JSON]
        (Headers '[Header "User-Count" Integer] [User])

type ProtectedAPI11 =
  UserAPI
    :<|> BasicAuth "my-realm" User
      :> UserAPI2

type UserAPI12 innerAPI =
  UserAPI
    :<|> "inner" :> innerAPI

type UserAPI12Alone = UserAPI12 EmptyAPI
