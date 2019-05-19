{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.Reader
import qualified Data.String.Class as S
import Data.Text (Text)
import GHC.Generics
import GHC.Stack
import Network.HTTP.Types.Status (status200)
import qualified Network.Wai as Wai
import Network.Wai (Request)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import UnliftIO.Exception (SomeException, try)

errorOut :: HasCallStack => IO Text
errorOut = do
  error "Erroring out!"

main :: IO ()
main = do
  let app :: Wai.Application
      app req respond =
        Control.Exception.bracket_
          (putStrLn "Allocating scarce resource")
          (putStrLn "Cleaning up")
          (respond $ error "Erroring out")
  let tlsSettings =
        WarpTLS.tlsSettings "../keys/certificate.pem" "../keys/key.pem"
  let warpSettings =
        Warp.setOnException onExceptionAct $
        Warp.setPort 8000 Warp.defaultSettings
  WarpTLS.runTLS tlsSettings warpSettings app
  -- Warp.runSettings warpSettings app

onExceptionAct :: HasCallStack => Maybe Wai.Request -> SomeException -> IO ()
onExceptionAct _mReq exc = do
  S.putStrLn
    ("Error: caught an exception: " <> tshow exc <> ". Call stack: " <>
     S.toText (prettyCallStack callStack))

tshow = S.toText . show
