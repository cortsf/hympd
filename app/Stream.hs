{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Stream where

import Data.String (fromString)
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent
import GHC.Float (int2Double)
import qualified Text.ParserCombinators.Parsec as P
import qualified Network.WebSockets as WS
import qualified Network.MPD as MPD
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Aeson as A
import qualified GHC.Generics as G


data Msg = Toggle 
  | Stop 
  | Next 
  | Previous 
  | Status 
  | Volume Int 
  | PlayId Int 
  | DeleteId Int 
  | PlayPath String 
  | AddPath String 
  | SeekCur Int
  deriving Show

parseMsg :: String -> Either P.ParseError Msg
parseMsg = P.parse (P.try parseCommandValue P.<|> P.try parseCommand) "(unknown)"

parseCommand :: P.Parser Msg
parseCommand = do
  command <- P.try (P.string "toggle") P.<|> 
             P.try (P.string "stop") P.<|> 
             P.try (P.string "previous") P.<|> 
             P.try (P.string "next")
  P.eof
  case command of
    "toggle" -> pure $ Toggle
    "stop" -> pure $ Stop
    "previous" -> pure $ Previous
    "next" -> pure $ Next

parseCommandValue :: P.Parser Msg
parseCommandValue = do
  command <- P.try (P.string "volume") P.<|> 
             P.try (P.string "deleteId") P.<|> 
             P.try (P.string "playId") P.<|> 
             P.try (P.string "playPath") P.<|> 
             P.try (P.string "addPath") P.<|>
             P.try (P.string "seekCur")
  P.char ','
  value <- P.many1 $ P.noneOf ","
  P.eof
  case command of
    "volume" -> pure $ Volume $ read value
    "deleteId" -> pure $ DeleteId $ read value
    "playId" -> pure $ PlayId $ read value
    "playPath" -> pure $ PlayPath $ value
    "addPath" -> pure $ AddPath $ value
    "seekCur" -> pure $ SeekCur $ read value

streamData :: MonadIO m => WS.PendingConnection -> m ()
streamData pc = do
  conn <- liftIO $ WS.acceptRequest pc
  liftIO $ WS.withPingThread conn 30 (return ()) $ do
    _  <- forkIO $ forever $ do 
      _subsystem <- MPD.withMPD $ MPD.idle [MPD.MixerS, MPD.PlayerS, MPD.PlaylistS]
      idle_status <- liftIO $ MPD.withMPD $ MPD.status
      sendStatus idle_status conn
    forever $ do
      msg <- WS.receiveData conn :: IO T.Text
      js_request_status <- liftIO $ MPD.withMPD $ MPD.status
      liftIO $ putStrLn "============="
      liftIO $ putStrLn $ T.unpack $ "action: '" <> msg <> "'"
      liftIO $ putStrLn $ "parsed action: '" <> (show $ parseMsg $ T.unpack msg) <> "'"
      case (parseMsg $ T.unpack msg, js_request_status) of
        (Right Toggle, Right js_request_status_nowrap)  -> (if MPD.stState js_request_status_nowrap == MPD.Stopped then (liftIO $ MPD.withMPD $ MPD.play Nothing) else  (liftIO $ MPD.withMPD $ MPD.toggle)) >> sendStatus js_request_status conn
        (Right Stop, Right _js_request_status_nowrap) -> (liftIO $ MPD.withMPD $ MPD.stop) >> sendStatus js_request_status conn
        (Right Next, Right _js_request_status_nowrap) -> (liftIO $ MPD.withMPD $ MPD.next) >> sendStatus js_request_status conn
        (Right Previous, Right _js_request_status_nowrap) -> (liftIO $ MPD.withMPD $ MPD.previous) >> sendStatus js_request_status conn
        (Right Status, Right _js_request_status_nowrap) -> sendStatus js_request_status conn
        (Right (Volume v), Right _js_request_status_nowrap) -> (liftIO $ MPD.withMPD $ MPD.setVolume (fromIntegral v)) >> sendStatus js_request_status conn
        (Right (PlayId v), Right _js_request_status_nowrap) -> (liftIO $ MPD.withMPD $ MPD.playId (MPD.Id v)) >> sendStatus js_request_status conn
        (Right (DeleteId v), Right _js_request_status_nowrap) -> (liftIO $ MPD.withMPD $ MPD.deleteId (MPD.Id v)) >> sendStatus js_request_status conn
        (Right (PlayPath v), Right _js_request_status_nowrap) -> (liftIO $ MPD.withMPD $ MPD.clear) >> (liftIO $ MPD.withMPD $ MPD.add (fromString v)) >> (liftIO $ MPD.withMPD $ MPD.play Nothing) >> sendStatus js_request_status conn
        (Right (AddPath v), Right _js_request_status_nowrap) -> (liftIO $ MPD.withMPD $ MPD.add (fromString v)) >> sendStatus js_request_status conn
        (Right (SeekCur v), Right js_request_status_nowrap) -> do
          case MPD.stTime js_request_status_nowrap of
            Nothing -> sendStatus js_request_status conn
            Just (_current_time, song_length) -> (liftIO $ MPD.withMPD $ MPD.seekCur True (((int2Double v)/100)*song_length)) >> sendStatus js_request_status conn
        _ -> sendStatus js_request_status conn
  where
    sendStatus status conn = do
      case status of
        Left _e -> WS.sendTextData conn ("response: " <> "error" :: T.Text)
        Right s -> WS.sendTextData conn (TLE.decodeUtf8 $ A.encode s)

deriving instance G.Generic MPD.Status

instance A.ToJSON MPD.PlaybackState where
  toJSON v = A.String $ T.pack $ show v

instance A.ToJSON MPD.Volume where
  toJSON v = A.String $ T.pack $ show $ toInteger v

instance A.ToJSON MPD.Id where
  toJSON (MPD.Id v) = A.String $ T.pack $ show v

instance A.ToJSON MPD.Status
