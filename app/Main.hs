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
import Utility

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import Lucid
import Servant.HTML.Lucid
import Servant.API.WebSocket
import Options.Applicative


parseArgs :: IO Options
parseArgs = execParser $ info confParser (progDesc "Hympd: MPD web client")

confParser :: Parser Options
confParser = do
  port <-
    option auto $
      long "port"
        <> metavar "INT"
        <> help "port (web interface)"
  mpdHost <-
    strOption $
      long "mpd-host"
        <> metavar "STRING"
        <> value "localhost"
        <> help "MPD host"
  mpdPort <-
    option auto $
      long "mpd-port"
        <> metavar "INT"
        <> value 6600
        <> help "MPD port"
  mpdPass <-
    strOption $
      long "mpd-password"
        <> metavar "STRING"
        <> value ""
        <> help "MPD password"
  pure $ Options {..}

main :: IO ()
main = do
  args <- parseArgs
  runServer $ args

-- * api

type WebApi =
  Get '[HTML] (Html ()) :<|>
  "queue" :> Get '[HTML] (Html ()) :<|>
  "browse" :> QueryParam "path" String :> Get '[HTML] (Html ()) :<|>
  "settings" :> Get '[HTML] (Html ()) :<|>
  "websocket" :> WebSocketPending :<|>
  "static" :> Raw

webApi :: Proxy WebApi
webApi = Proxy

-- * app

server :: Options -> Server WebApi
server options = 
  (queuePage options) :<|> 
  (queuePage options) :<|> 
  (browsePage options) :<|> 
  (settingsPage options) :<|> 
  (streamData options) :<|> 
  (serveDirectoryWebApp "static")


runServer :: Options -> IO ()
runServer options = do
  let settings =
        setPort (port options) $
        setBeforeMainLoop (hPutStrLn stderr ("Listening on port " ++ show (port options))) $
        defaultSettings
  runSettings settings =<< (mkApp options)


mkApp :: Options -> IO Application
mkApp options = do
  pure $ serve webApi (server options)
