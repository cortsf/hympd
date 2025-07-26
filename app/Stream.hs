{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Stream where

-- import Utility ()
import Utility (guessTitle)
import Data.String (fromString)
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent
import GHC.Float (int2Double)
import qualified Text.ParserCombinators.Parsec as P
import qualified Network.WebSockets as WS
import qualified Network.MPD as MPD
import qualified Data.Text as T
-- import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Aeson as A
import qualified GHC.Generics as G


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
  | Volume Int 
  | PlayId Int 
  | DeleteId Int 
  | PlayPath String 
  | AddPath String 
  | SeekCur Int
  deriving Show

parseMsg :: String -> Either P.ParseError MPDCommand
parseMsg = P.parse (parseCommand P.<|> parseCommandValue) "(unknown)"

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
             P.try (P.string "consume")
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

parseCommandValue :: P.Parser MPDCommand
parseCommandValue = do
  command <- P.try (P.string "volume") P.<|> 
             P.try (P.string "deleteId") P.<|> 
             P.try (P.string "playId") P.<|> 
             P.try (P.string "playPath") P.<|> 
             P.try (P.string "addPath") P.<|>
             P.try (P.string "seekCur")
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


data ClientMessage = 
  IdleUpdate [MPD.Subsystem] MPD.Status (Maybe String)
  | ClientResponse MPD.Status
  | Error String
  deriving (Show, G.Generic)

deriving instance G.Generic MPD.Subsystem
instance A.ToJSON MPD.Subsystem

instance A.ToJSON ClientMessage where
    toEncoding = A.genericToEncoding $ A.defaultOptions { A.sumEncoding = A.TaggedObject { tagFieldName = "payloadType", contentsFieldName = "payload" } }

streamData :: MonadIO m => WS.PendingConnection -> m ()
streamData pc = do
  conn <- liftIO $ WS.acceptRequest pc
  liftIO $ WS.withPingThread conn 30 (return ()) $ do
    _  <- forkIO $ forever $ do 
      idle_subsystemsResponse <- MPD.withMPD $ MPD.idle [MPD.MixerS, MPD.PlayerS, MPD.PlaylistS, MPD.OptionsS]
      case idle_subsystemsResponse of
        Left mpd_error -> sendMessage (Error $ "Idle error: " <> show mpd_error) conn
        Right subsystems -> do
          idle_statusResponse <- liftIO $ MPD.withMPD $ MPD.status
          case idle_statusResponse of
            Left status_error -> sendMessage (Error $ "Status error (idle): " <> show status_error) conn
            Right status -> do
              currentSong <- MPD.withMPD $ MPD.currentSong
              case currentSong of
                Left song_error -> sendMessage (Error $ "Song error (idle) " <> show song_error) conn
                -- Right song -> sendMessage (IdleUpdate subsystems status (show <$> song)) conn
                Right song -> sendMessage (IdleUpdate subsystems status (guessTitle <$> song)) conn
      pure ()
    forever $ do
      msg <- WS.receiveData conn :: IO T.Text
      js_request_status <- liftIO $ MPD.withMPD $ MPD.status
      liftIO $ putStrLn "============="
      liftIO $ putStrLn $ T.unpack $ "action: '" <> msg <> "'"
      liftIO $ putStrLn $ "parsed action: '" <> (show $ parseMsg $ T.unpack msg) <> "'"
      case (parseMsg $ T.unpack msg, js_request_status) of
        (Right Status, Right js_request_status_nowrap) ->do
          liftIO $ sendMessage (ClientResponse js_request_status_nowrap) conn
          pure ()
        (Right Toggle, Right js_request_status_nowrap) -> do
          (if MPD.stState js_request_status_nowrap == MPD.Stopped then (liftIO $ MPD.withMPD $ MPD.play Nothing) else  (liftIO $ MPD.withMPD $ MPD.toggle)) 
          pure ()
        (Right Stop, Right _js_request_status_nowrap) -> do
          (liftIO $ MPD.withMPD $ MPD.stop) 
          pure ()
        (Right Next, Right _js_request_status_nowrap) -> do
          (liftIO $ MPD.withMPD $ MPD.next) 
          pure ()
        (Right Previous, Right _js_request_status_nowrap) -> do
          (liftIO $ MPD.withMPD $ MPD.previous) 
          pure ()
        (Right Clear, Right _js_request_status_nowrap) -> do
          (liftIO $ MPD.withMPD $ MPD.clear) 
          pure ()
        (Right Random, Right js_request_status_nowrap) -> do
          (liftIO $ MPD.withMPD $ MPD.random $ not $ MPD.stRandom $ js_request_status_nowrap) 
          pure ()
        (Right Repeat, Right js_request_status_nowrap) -> do
          (liftIO $ MPD.withMPD $ MPD.repeat $ not $ MPD.stRepeat $ js_request_status_nowrap) 
          pure ()
        (Right Single, Right js_request_status_nowrap) -> do
          (liftIO $ MPD.withMPD $ MPD.single $ not $ MPD.stSingle $ js_request_status_nowrap) 
          pure ()
        (Right Consume, Right js_request_status_nowrap) -> do
          (liftIO $ MPD.withMPD $ MPD.consume $ not $ MPD.stConsume $ js_request_status_nowrap) 
          pure ()
        (Right (Volume v), Right _js_request_status_nowrap) -> do
          (liftIO $ MPD.withMPD $ MPD.setVolume (fromIntegral v)) 
          pure ()
        (Right (PlayId v), Right _js_request_status_nowrap) -> do
          (liftIO $ MPD.withMPD $ MPD.playId (MPD.Id v)) 
          pure ()
        (Right (DeleteId v), Right _js_request_status_nowrap) -> do
          (liftIO $ MPD.withMPD $ MPD.deleteId (MPD.Id v)) 
          pure ()
        (Right (PlayPath v), Right _js_request_status_nowrap) -> do
          (liftIO $ MPD.withMPD $ MPD.clear) >> (liftIO $ MPD.withMPD $ MPD.add (fromString v)) >> (liftIO $ MPD.withMPD $ MPD.play Nothing) 
          pure ()
        (Right (AddPath v), Right _js_request_status_nowrap) -> do
          (liftIO $ MPD.withMPD $ MPD.add (fromString v)) 
          pure ()
        (Right (SeekCur v), Right js_request_status_nowrap) -> do
          case MPD.stTime js_request_status_nowrap of
            Nothing -> sendMessage (ClientResponse js_request_status_nowrap) conn >> pure ()
            Just (_current_time, song_length) -> (liftIO $ MPD.withMPD $ MPD.seekCur True (((int2Double v)/100)*song_length)) >> pure ()
        (Left parse_error, _) -> do
          sendMessage (Error $ "Parse request error: " <> show parse_error) conn 
          pure ()
        (_, Left mpd_error) -> do
          sendMessage (Error $ "MPD Error: " <> show mpd_error) conn 
          pure ()
  where
    sendMessage msg conn = do
      WS.sendTextData conn (A.encode msg)
