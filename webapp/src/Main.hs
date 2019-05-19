{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.Reader
import qualified Data.String.Class as S
import Data.Text (Text)
import GHC.Generics
import GHC.Stack
import qualified Network.Wai as Wai
import Network.Wai (Request)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
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
import System.IO
import UnliftIO.Exception (SomeException, try)
import qualified Control.Exception

data Env =
  Env

type AppM = ReaderT Env IO

type FullAPI = ToServantApi API

data API route =
  API
    { _index :: route :- Get '[ PlainText] Text
    , _ping :: route :- "api" :> "ping" :> Get '[ PlainText] Text
    , _errorOut :: route :- "api" :> "error-out" :> Get '[ PlainText] Text
    }
  deriving (Generic)

api :: Proxy (ToServantApi API)
api = genericApi (Proxy :: Proxy API)

fullApi :: Proxy FullAPI
fullApi = Proxy

errorOut :: HasCallStack => AppM Text
errorOut = do
  error "Erroring out!"

server :: API (AsServerT AppM)
server = API {_index = pure "hey", _ping = pure "pong", _errorOut = errorOut}

fullServer :: ServerT FullAPI AppM
fullServer = genericServerT server

nt :: Env -> AppM a -> Servant.Handler a
nt s x = Servant.Handler $ ExceptT $ try $ runReaderT x s

type UserId = Int

auth :: Context (AuthHandler Request UserId ': '[])
auth = (mkAuthHandler (const (pure 1))) :. EmptyContext

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  let pureApp :: Wai.Application
      pureApp req respond =
        Control.Exception.bracket_
          (putStrLn "Allocating scarce resource")
          (putStrLn "Cleaning up")
          (error "Erroring out")
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
  let tlsSettings =
        WarpTLS.tlsSettings "../keys/certificate.pem" "../keys/key.pem"
  let warpSettings =
        Warp.setOnException onExceptionAct $
        Warp.setPort 8000 Warp.defaultSettings
  -- WarpTLS.runTLS tlsSettings warpSettings app
  WarpTLS.runTLS tlsSettings warpSettings pureApp
  -- Warp.runSettings warpSettings app

onExceptionAct :: HasCallStack => Maybe Wai.Request -> SomeException -> IO ()
onExceptionAct _mReq exc = do
  S.putStrLn
    ("Error: caught an exception: " <> tshow exc <> ". Call stack: " <>
     S.toText (prettyCallStack callStack))

tshow = S.toText . show
