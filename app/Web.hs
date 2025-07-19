{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
module Web where

import Javascript

import Data.Aeson qualified as A
import Data.Aeson.Text qualified as A
import Control.Monad.IO.Class
import Data.Maybe (listToMaybe, maybeToList)
import Data.Time.Clock.POSIX
import Data.String(fromString)
import Data.Time.Format
import Data.List (sortBy)
import Data.Coerce (coerce)
import Lucid
import Servant (Handler)
import GHC.Generics
-- import Data.Char (chr)
import qualified Data.Text as T
import qualified Network.MPD as MPD
import qualified Data.Map.Strict as C
import qualified System.FilePath.Posix as F

------------------------------------------------------------
-- Common
------------------------------------------------------------
-- data SongList = SongList { songId :: Int, songName :: String } deriving Generic
-- instance A.ToJSON SongList

page :: Html () -> Handler (Html ())
page content = do
  pure $ html_ $ do
    head_ $ do
      title_ "Hympd"
      script_ [src_ "static/styles.css"] ("" :: String)
      script_ [src_ "static/icons.js"] ("" :: String)
      link_ [rel_ "shortcut icon", href_ "data:,"]
    body_ [class_ "overflow-y-scroll flex flex-col h-screen bg-blue-100"] $ do
      nav
      div_ [class_ "max-w-screen-xl w-full flex flex-wrap flex-col flex-grow mx-auto p-4 pt-18 pb-20 bg-white"] $ do
        content
      footer
      script_ $ "feather.replace();"
      script_ $ jsblock
    


nav :: Html ()
nav = nav_ [class_ "bg-white border-gray-200 dark:bg-gray-900 fixed w-full"] $ do
  div_ [class_ "max-w-screen-xl flex flex-wrap items-center justify-between mx-auto p-4"] $ do
    div_ [class_ "hidden w-full md:block md:w-auto", id_ "navbar-default"] $ do
      ul_ [class_ "font-medium flex flex-col p-4 md:p-0 mt-4 border border-gray-100 rounded-lg md:flex-row md:space-x-8 rtl:space-x-reverse md:mt-0 md:border-0 dark:border-gray-700"] $ do
        li_ $ a_ [href_ "/queue", class_ "block py-2 px-3 text-white bg-blue-700 rounded-sm md:bg-transparent md:text-blue-700 md:p-0 dark:text-white md:dark:text-blue-500 hover:text-blue-200"] "Queue"
        li_ $ a_ [href_ "/browse", class_ "block py-2 px-3 text-white bg-blue-700 rounded-sm md:bg-transparent md:text-blue-700 md:p-0 dark:text-white md:dark:text-blue-500 hover:text-blue-200"] "Browse"
        li_ $ a_ [href_ "/settings", class_ "block py-2 px-3 text-white bg-blue-700 rounded-sm md:bg-transparent md:text-blue-700 md:p-0 dark:text-white md:dark:text-blue-500 hover:text-blue-200"] "Settings"
    div_ [class_ "flex space-x-4"] $ do
      button_ [id_ "navPrevious", class_ "block py-2 text-white bg-blue-200 rounded-sm md:bg-transparent md:text-blue-700 md:p-0 dark:text-white md:dark:text-blue-500 hover:text-blue-200"] $ i_ [data_ "feather" "skip-back"] ""
      button_ [id_ "navStop", class_ "block py-2 text-white bg-blue-200 rounded-sm md:bg-transparent md:text-blue-700 md:p-0 dark:text-white md:dark:text-blue-500 hover:text-blue-200"] $ i_ [data_ "feather" "square"] ""
      button_ [id_ "navPlayPause", class_ "block py-2 text-white bg-blue-200 rounded-sm md:bg-transparent md:text-blue-700 md:p-0 dark:text-white md:dark:text-blue-500 hover:text-blue-200"] $ ""
      button_ [id_ "navNext", class_ "block py-2 text-white bg-blue-200 rounded-sm md:bg-transparent md:text-blue-700 md:p-0 dark:text-white md:dark:text-blue-500 hover:text-blue-200"] $ i_ [data_ "feather" "skip-forward"] ""
      div_ [class_ "flex items-center"] $ input_ [id_ "navVolume", onchange_ "socket.send('volume,' + this.value)", type_ "range", value_ "0", class_ "w-full h-1 bg-gray-200 rounded-lg appearance-none cursor-pointer dark:bg-gray-700"]

footer :: Html ()
footer = div_ [class_ "bg-white border-gray-200 dark:bg-slate-600 fixed bottom-0 w-full py-4"] $ do
  div_ [class_ "max-w-screen-xl w-full mx-auto"] $ do 
    h1_ [id_ "currentSong", class_ "text-2xl text-blue-200"] $ ".."
  div_ [class_ "max-w-screen-xl flex items-center justify-between mx-auto"] $ do
    input_ [id_ "footerProgress", oninput_ "socket.send('seekCur,'+this.value)", type_ "range", value_ "0", class_ "mt-2 w-full h-1 bg-gray-200 rounded-lg cursor-pointer dark:bg-gray-700"]


------------------------------------------------------------
-- Individual Pages
------------------------------------------------------------
unId :: MPD.Id -> Int
unId (MPD.Id n) = n

queuePage :: Handler (Html ())
queuePage = do
  playlist <- liftIO $ MPD.withMPD $ MPD.playlistInfo Nothing
  page $ do
    p_ [class_ "text-2xl"] "Queue"
    case playlist of
      Left _ -> p_ "playlist error"
      Right pl -> table_ [class_ "table-auto w-full mt-4"] $ do
        tbody_ $ mapM_ (\song ->
                          tr_ [class_ "my-0 odd:bg-white even:bg-slate-50 hover:bg-sky-100 flex place-content-between"] $ do
                           td_ [class_ "text-gray-500 flex items-center place-content-start w-8"] $ toHtml $ maybe "" (show . (+1)) (MPD.sgIndex song)
                           td_ [onclick_ (maybe "alert('Error: No id')" (\x -> "socket.send('playId," <> (T.pack $ show $ unId x) <> "')")  (MPD.sgId song)),  class_ "py-2 flex place-content-between flex-grow hover:text-sky-600"] $ do
                             -- div_ ([class_ "text-ellipsis"] <> (maybe [] (\songId -> [(data_ . "song-id" songId)] MPD.stSongID song))) $ toHtml $ 
                             div_ ([class_ "text-ellipsis songId"] <> ((\songId -> data_ "songId" (T.pack $ show $ (unId songId))) <$> (maybeToList $ MPD.sgId song))) $ toHtml $ 
                               maybe
                               (F.takeBaseName $ MPD.toString $ MPD.sgFilePath song)
                               (\x -> maybe "No title metadata" MPD.toString (listToMaybe x))
                               (C.lookup MPD.Title (MPD.sgTags song))
                             div_ [class_ "px-2"] $ toHtml $ formatTime defaultTimeLocale (if MPD.sgLength song > 3600 then "%H:%M:%S" else "%M:%S") $ posixSecondsToUTCTime $ fromIntegral $ MPD.sgLength song
                           td_ [class_ "flex gap-x-2 pr-2"] $ do
                             button_ [onclick_ $ "location.href='/browse?path=" <> (MPD.toText $ MPD.sgFilePath song) <> "'", class_ "pl-0 pr-4 w-1 text-red-300 hover:text-red-400 hover:cursor-pointer"] $ i_ [class_ "size-4 stroke-2", data_ "feather" "search"] ""
                             button_ [onclick_ (maybe "alert('Error: No id')" (\x -> "socket.send('deleteId," <> (T.pack $ show $ unId x) <> "')")  (MPD.sgId song)), class_ "pl-0 pr-4 w-1 text-red-300 hover:text-red-400 hover:cursor-pointer"] $ i_ [class_ "size-4 stroke-2", data_ "feather" "trash-2"] ""
                       ) pl

browsePage :: Maybe String -> Handler (Html ())
browsePage query_path = do
  mpdResult <- liftIO $ MPD.withMPD $ MPD.lsInfo $ maybe "" (fromString . id) query_path
  page $ do
    p_ [class_ "text-2xl"] "Browse"
    case mpdResult of
      Left e -> p_ "Browse error" <> p_ (toHtml $ show e)
      Right res -> table_ [class_ "table-auto w-full mt-4"] $ do
        tbody_ $ mapM_ (\item -> tr_ [class_ "my-0 odd:bg-white even:bg-slate-50 hover:bg-sky-100 flex place-content-between"] $ do
                           case item of
                             MPD.LsDirectory path -> do
                               mkItemIcon "folder"
                               td_ [class_ "pl-4 py-2 overflow-hidden grow hover:cursor-pointer", onclick_ $ "location.href='" <> (("/browse?path=" :: T.Text) <> (MPD.toText path)) <> "'"] $ toHtml $ MPD.toString path
                               mkQueueButtons(T.pack $ MPD.toString path)
                             MPD.LsSong song -> do
                               mkItemIcon "music"
                               td_ [class_ "pl-4 py-2 overflow-hidden grow flex place-content-between"] $ do
                                 div_ $ toHtml $ maybe (F.takeBaseName $ MPD.toString $ MPD.sgFilePath song) (\x -> maybe "No title metadata msg2" MPD.toString (listToMaybe x)) (C.lookup MPD.Title (MPD.sgTags song))
                                 div_ [class_ "px-2"] $ toHtml $ formatTime defaultTimeLocale (if MPD.sgLength song > 3600 then "%H:%M:%S" else "%M:%S") $ posixSecondsToUTCTime $ fromIntegral $ MPD.sgLength song
                               mkQueueButtons(T.pack $ MPD.toString $ MPD.sgFilePath song)
                             MPD.LsPlaylist playlist -> do 
                               mkItemIcon "list"
                               td_ [class_ "pl-4 py-2 overflow-hidden grow"] $ toHtml $ MPD.toString playlist
                               mkQueueButtons(T.pack $ MPD.toString $ playlist)
                       ) (sortBy (\x y -> 
                                    case (x, y) of
                                      (MPD.LsSong _, MPD.LsSong _) -> EQ
                                      (MPD.LsSong _, MPD.LsPlaylist _) -> GT
                                      (MPD.LsSong _, MPD.LsDirectory _) -> GT
                                      (MPD.LsPlaylist _, MPD.LsSong _) -> LT
                                      (MPD.LsPlaylist _, MPD.LsPlaylist _) -> EQ
                                      (MPD.LsPlaylist _, MPD.LsDirectory _) -> GT
                                      (MPD.LsDirectory _, MPD.LsSong _) -> LT
                                      (MPD.LsDirectory _, MPD.LsPlaylist _) -> LT
                                      (MPD.LsDirectory _, MPD.LsDirectory _) -> EQ
                                 ) res)
      
  where
    mkItemIcon :: T.Text -> Html ()
    mkItemIcon icon = td_ [class_ "text-slate-400 flex items-center"] $ i_ [class_ "size-4", data_ "feather" icon] ""
    mkQueueButtons :: T.Text -> Html ()
    mkQueueButtons path = td_ [class_ "py-2 text-cyan-600 flex gap-x-2 pr-2"] $ do 
      button_ [onclick_ $ "socket.send('addPath," <> path <> "')", class_ "hover:text-cyan-200"] $ i_ [class_ "size-5 stroke-3", data_ "feather" "plus"] "__"
      button_ [onclick_ $ "socket.send('playPath," <> path <> "')", class_ "hover:text-cyan-200"] $ i_ [class_ "size-5 stroke-3", data_ "feather" "play"] "__"

settingsPage :: Handler (Html ())
settingsPage = do
  mpdResult <- liftIO $ MPD.withMPD $ MPD.config
  mpdStatus <- liftIO $ MPD.withMPD $ MPD.status
  page $ do
    p_ [class_ "text-2xl"] "Settings"
    div_ [class_ "bg-gray-200 mt-4 p-4"] $ toHtml $ show mpdStatus
    div_ [class_ "bg-blue-200 mt-4 w-full"] $ toHtml $ show mpdResult
