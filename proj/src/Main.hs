{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Reader
import Data.Text (Text)
import GHC.Generics
import Network.Wai (Request)
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant
import Servant.API.Generic
import Servant.Server
import Servant.Server.Experimental.Auth
  ( AuthHandler
  , AuthServerData
  , mkAuthHandler
  )
import Servant.Server.Experimental.Auth (AuthHandler)
import Servant.Server.Generic (AsServerT, genericServerT)

data Env =
  Env

type AppM = ReaderT Env Servant.Handler

type FullAPI = ToServantApi API

data API route =
  API
    { _index :: route :- Get '[ PlainText] Text
    , _ping :: route :- "api" :> "ping" :> Get '[ PlainText] Text
    }
  deriving (Generic)

api :: Proxy (ToServantApi API)
api = genericApi (Proxy :: Proxy API)

fullApi :: Proxy FullAPI
fullApi = Proxy

server :: API (AsServerT AppM)
server = API {_index = pure "hey", _ping = pure "pong"}

fullServer :: ServerT FullAPI AppM
fullServer = genericServerT server

nt :: Env -> AppM a -> Servant.Handler a
nt s x = runReaderT x s

type UserId = Int

auth :: Context (AuthHandler Request UserId ': '[])
auth = (mkAuthHandler (const (pure 1))) :. EmptyContext

main :: IO ()
main = do
  let env = Env
      hoisted :: ServerT FullAPI Servant.Handler
      hoisted =
        (hoistServerWithContext
           (Proxy :: Proxy FullAPI)
           (Proxy :: Proxy '[ UserId])
           (nt env)
           fullServer)
      app :: Application
      app = serveWithContext (Proxy :: Proxy FullAPI) auth hoisted
  Warp.run 8000 app
