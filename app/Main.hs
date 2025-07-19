{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Main where
import Web 
import Stream

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import Lucid
import Servant.HTML.Lucid
import Servant.API.WebSocket

main :: IO ()
main = do
  runServer

-- * api

type WebApi =
  "queue" :> Get '[HTML] (Html ()) :<|>
  "browse" :> QueryParam "path" String :> Get '[HTML] (Html ()) :<|>
  "settings" :> Get '[HTML] (Html ()) :<|>
  "websocket" :> WebSocketPending :<|>
  "static" :> Raw

webApi :: Proxy WebApi
webApi = Proxy

-- * app

server :: Server WebApi
server = queuePage :<|> 
         browsePage :<|> 
         settingsPage :<|> 
         streamData :<|> 
         (serveDirectoryWebApp "static")


runServer :: IO ()
runServer = do
  let port = 3009
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("Listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve webApi server
