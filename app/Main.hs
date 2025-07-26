{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
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
import Options.Applicative

data Config = Config
  { port :: Int
  }
  deriving (Show)

parseArgs :: IO Config
parseArgs = execParser $ info confParser (progDesc "Hympd: MPD web client")

confParser :: Parser Config
confParser = do
  port <-
    option auto $
      long "port"
        <> metavar "INT"
        <> help "port (web interface)"
  pure $ Config {..}

main :: IO ()
main = do
  args <- parseArgs
  runServer $ port args

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


runServer :: Int -> IO ()
runServer port = do
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("Listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = do
  pure $ serve webApi server
