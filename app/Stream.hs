{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Stream where

import Utility (withMpdOpt, Options(..), CurrentSong(..), currentSongFromSong)
import Data.String (fromString)
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent
import GHC.Float (int2Double)
import Text.ParserCombinators.Parsec qualified as P
import Network.WebSockets qualified as WS
import Network.MPD qualified as MPD
import Data.Text qualified as T
import Data.Aeson qualified as A
import GHC.Generics qualified as G


data MPDCommand = Toggle 
  | Stop 
  | Next 
  | Previous 
  | Status 
  | Clear
  | Random
  | Repeat
  | Single 
  | Consume
  | UpdateAll
  | Volume Int 
  | PlayId Int 
  | DeleteId Int 
  | PlayPath String 
  | AddPath String 
  | SeekCur Int
  | Update String
  deriving Show

parseMsg :: String -> Either P.ParseError MPDCommand
parseMsg = P.parse (parseCommand P.<|> parseCommandValue) "Can't parse input."

parseCommand :: P.Parser MPDCommand
parseCommand = do
  command <- P.try (P.string "toggle") P.<|> 
             P.try (P.string "stop") P.<|> 
             P.try (P.string "previous") P.<|> 
             P.try (P.string "next") P.<|>
             P.try (P.string "status") P.<|>
             P.try (P.string "clear") P.<|>
             P.try (P.string "random") P.<|>
             P.try (P.string "repeat") P.<|>
             P.try (P.string "single") P.<|>
             P.try (P.string "consume") P.<|>
             P.try (P.string "updateAll")
  P.eof
  case command of
    "toggle" -> pure $ Toggle
    "stop" -> pure $ Stop
    "previous" -> pure $ Previous
    "next" -> pure $ Next
    "status" -> pure $ Status
    "clear" -> pure $ Clear
    "random" -> pure $ Random
    "repeat" -> pure $ Repeat
    "single" -> pure $ Single
    "consume" -> pure $ Consume
    "updateAll" -> pure $ UpdateAll

parseCommandValue :: P.Parser MPDCommand
parseCommandValue = do
  command <- P.try (P.string "volume") P.<|> 
             P.try (P.string "deleteId") P.<|> 
             P.try (P.string "playId") P.<|> 
             P.try (P.string "playPath") P.<|> 
             P.try (P.string "addPath") P.<|>
             P.try (P.string "seekCur") P.<|>
             P.try (P.string "update")
  P.char ','
  value <- P.many1 $ P.anyChar
  P.eof
  case command of
    "volume" -> pure $ Volume $ read value
    "deleteId" -> pure $ DeleteId $ read value
    "playId" -> pure $ PlayId $ read value
    "playPath" -> pure $ PlayPath $ value
    "addPath" -> pure $ AddPath $ value
    "seekCur" -> pure $ SeekCur $ read value
    "update" -> pure $ Update $ value

data ClientMessage = 
  -- Payload [MPD.Subsystem] MPD.Status (Maybe String)
  Payload [MPD.Subsystem] MPD.Status (Maybe CurrentSong)
  | Error String
  deriving (Show, G.Generic)

deriving instance G.Generic MPD.Subsystem
instance A.ToJSON MPD.Subsystem

instance A.ToJSON ClientMessage where
    toEncoding = A.genericToEncoding $ 
      A.defaultOptions { A.sumEncoding = A.TaggedObject { tagFieldName = "payloadType", contentsFieldName = "payload" }}

streamData :: MonadIO m => Options -> WS.PendingConnection -> m ()
streamData options pc = do
  conn <- liftIO $ WS.acceptRequest pc
  liftIO $ WS.withPingThread conn 30 (return ()) $ do
    _  <- forkIO $ forever $ do 
      idle_subsystemsResponse <- withMpdOpt options $ MPD.idle [MPD.MixerS, MPD.PlayerS, MPD.PlaylistS, MPD.OptionsS]
      case idle_subsystemsResponse of
        Left mpd_error -> sendError conn $ "Idle error: " <> show mpd_error
        Right subsystems -> do
          onStatus conn (sendData conn subsystems)
      pure ()
    forever $ do
      msg <- WS.receiveData conn :: IO T.Text
      liftIO $ putStrLn "============="
      liftIO $ putStrLn $ T.unpack $ "action: '" <> msg <> "'"
      liftIO $ putStrLn $ "parsed action: '" <> (show $ parseMsg $ T.unpack msg) <> "'"
      case parseMsg $ T.unpack msg of
        Right Status -> do
          onStatus conn (sendData conn [])
        Right Toggle -> do
          onStatus conn (\mpdStatus -> if MPD.stState mpdStatus == MPD.Stopped then (liftIO $ MPD.withMPD $ MPD.play Nothing) >> pure () else (liftIO $ MPD.withMPD $ MPD.toggle) >> pure ())
        Right Stop -> do
          liftIO $ MPD.withMPD $ MPD.stop
          pure ()
        Right Next -> do
          liftIO $ MPD.withMPD $ MPD.next
          pure ()
        Right Previous -> do
          liftIO $ MPD.withMPD $ MPD.previous
          pure ()
        Right Clear -> do
          liftIO $ MPD.withMPD $ MPD.clear
          pure ()
        Right Random -> do
          onStatus conn (\mpdStatus -> (liftIO $ MPD.withMPD $ MPD.random $ not $ MPD.stRandom mpdStatus) >> pure ())
        Right Repeat -> do
          onStatus conn (\mpdStatus -> (liftIO $ MPD.withMPD $ MPD.repeat $ not $ MPD.stRepeat $ mpdStatus) >> pure ())
        Right Single -> do
          onStatus conn (\mpdStatus -> (liftIO $ MPD.withMPD $ MPD.single $ not $ MPD.stSingle $ mpdStatus) >> pure ())
        Right Consume -> do
          onStatus conn (\mpdStatus -> (liftIO $ MPD.withMPD $ MPD.consume $ not $ MPD.stConsume $ mpdStatus) >> pure ())
        Right UpdateAll -> do
          liftIO $ MPD.withMPD $ MPD.update Nothing
          pure ()
        Right (Volume v) -> do
          liftIO $ MPD.withMPD $ MPD.setVolume (fromIntegral v)
          pure ()
        Right (PlayId v) -> do
          liftIO $ MPD.withMPD $ MPD.playId (MPD.Id v)
          pure ()
        Right (DeleteId v) -> do
          liftIO $ MPD.withMPD $ MPD.deleteId (MPD.Id v)
          pure ()
        Right (PlayPath v) -> do
          liftIO $ MPD.withMPD $ MPD.clear
          liftIO $ MPD.withMPD $ MPD.add (fromString v)
          liftIO $ MPD.withMPD $ MPD.play Nothing
          pure ()
        Right (AddPath v) -> do
          (liftIO $ MPD.withMPD $ MPD.add (fromString v)) 
          pure ()
        Right (SeekCur v) -> do
          onStatus conn (\mpdStatus -> maybe 
                                       ((sendError conn $ "Error: not playing") >> pure ())
                                       (\(_current_time, song_length) -> (liftIO $ MPD.withMPD $ MPD.seekCur True (((int2Double v)/100)*song_length)) >> pure ())
                                       (MPD.stTime mpdStatus))
        Right (Update v) -> do
          liftIO $ MPD.withMPD $ MPD.update $ Just $ fromString v
          pure ()
        Left parse_error -> do
          sendError conn $ "Parse request error: " <> show parse_error
          pure ()
  where
    onStatus conn fun = do
      statusResponse <- liftIO $ MPD.withMPD $ MPD.status
      case statusResponse of
        Left _error -> sendError conn $ "Can't get MPD status"
        Right status -> fun status
    sendError conn error_msg = do
      WS.sendTextData conn $ A.encode $ Error $ error_msg
    sendData conn subsystems status = if ((MPD.stState status) /= MPD.Stopped) then do
      currentSong <- MPD.withMPD MPD.currentSong
      case currentSong of
        Left song_error -> sendError conn $ "Song error: " <> show song_error
        Right song -> WS.sendTextData conn $ A.encode $ Payload subsystems status (currentSongFromSong <$> song)
      else
        WS.sendTextData conn $ A.encode $ Payload subsystems status Nothing
