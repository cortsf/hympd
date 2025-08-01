{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
module Web where

import Javascript
import Utility

import GHC.Float (double2Int)
import Data.Aeson qualified as A
import Data.Aeson.Text qualified as A
import Control.Monad.IO.Class
import Data.Maybe (listToMaybe, maybeToList, fromMaybe, isJust)
import Data.Time.Clock.POSIX
import Data.String(fromString)
import Data.Time.Format
import Data.List (sortBy, intersperse, zip4)
import Data.Coerce (coerce)
import Lucid
import Servant (Handler)
import GHC.Generics
import Network.URI.Encode qualified as U
import Data.Text qualified as T
import Text.Pretty.Simple qualified as P
import Network.MPD qualified as MPD
import Data.Map.Strict qualified as C
import System.FilePath.Posix qualified as FP

------------------------------------------------------------
-- Common
------------------------------------------------------------

data CurrentPage = Queue | Browse | Settings deriving Eq

page :: Options -> UserConfig -> CurrentPage -> Html () -> Handler (Html ())
page options user_config current_page content = do
  mpdStatus <- liftIO $ withMpdOpt options $ MPD.status
  case mpdStatus of
    Left e -> pure $ p_ $ toHtml $ "Error - Can't connect to MPD: "  <> show e
    Right status -> do
      currentSongResponse <- liftIO $ withMpdOpt options $ MPD.currentSong
      let current_time_percentage = maybe "0" (\time -> T.pack $ show $ ((fst time / snd time) * 100)) (MPD.stTime status)
          elapsed_time = maybe "0"  
            (prettyTime . double2Int . fst)
            (MPD.stTime status)
          total_time = maybe "0"  
            (prettyTime . double2Int . snd)
            (MPD.stTime status)
          playPause_icon = if MPD.stState status == MPD.Paused then "play" else "pause"
          volume = maybe 0 toInteger $ MPD.stVolume status
          current_song = do
            case currentSongResponse of
              Left _ -> Nothing
              Right Nothing -> Nothing
              Right msong -> currentSongFromSong <$> msong
          playbackState = MPD.stState status
      pure $ doctypehtml_ $ html_ [lang_ "en"] $ do
        head_ $ do
          title_ "Hympd"
          script_ [src_ "/static/styles.css"] ("" :: String)
          script_ [src_ "/static/icons.js"] ("" :: String)
          link_ [rel_ "icon", href_ "/static/favicon.png", sizes_ "any", type_ "image/png"]
          link_ [rel_ "manifest", href_ "/static/manifest.json"]
          style_ $ T.pack "#playerProgressInput{-webkit-appearance: none; background: oklch(55.6% 0 0); background-image: linear-gradient(#FFD6A8, #FFD6A8); background-size: " <> current_time_percentage <> "% 100%; background-repeat: no-repeat;}#playerProgressInput::-webkit-slider-thumb {-webkit-appearance: none; height: 0px; width: 0px;}"
          meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
          meta_ [name_ "description", content_ "Hympd: MPD client"]
        body_ [class_ "overflow-y-scroll"] $ do
          div_ [class_ "min-h-screen bg-red-200 flex flex-col bg-white md:bg-gray-200 dark:bg-gray-900 text-slate-600 dark:text-slate-400 lg:text-base wrap-anywhere focus:outline-none"] $ do
            nav_full current_page user_config current_song volume elapsed_time total_time playbackState playPause_icon
            div_ [id_ "content", class_ "overflow-y-visible max-w-screen-xl w-full grow flex flex-col mx-auto pt-4 bg-white dark:bg-slate-800 [&_tr]:odd:bg-slate-50 [&_tr]:odd:dark:bg-slate-700 [&_tr]:even:bg-white [&_tr]:even:dark:bg-slate-800 [&_tr]:dark:hover:bg-sky-900"] $ do
              content
            script_ $ "feather.replace();"
          script_ $ jsblock

nav_full :: CurrentPage -> UserConfig -> Maybe CurrentSong -> Integer -> String -> String -> MPD.PlaybackState -> String -> Html ()
nav_full current_page user_config current_song volume elapsed_time total_time playbackState playPause_icon = do
  nav_ [class_ "sticky top-0 w-full dark:text-blue-200 bg-slate-700 "] $ do
    div_ [class_ "max-w-screen-xl w-full flex block mx-auto lg:pt-4 px-2 lg:px-4 [&_.menuButton]:dark:hover:text-yellow-500"] $ do
      ul_ [class_ "font-medium flex flex-row space-x-8 w-full place-content-between lg:place-content-start"] $ do
        li_ $ a_ [href_ "/queue", classes_ [if current_page == Queue then "text-yellow-500" else "hover:text-blue-200 text-blue-200 dark:text-blue-200 menuButton", "block py-2 px-3 rounded-sm lg:p-0"]] "Queue"
        li_ $ a_ [href_ "/browse", classes_ [if current_page == Browse then "text-yellow-500" else "hover:text-blue-200 text-blue-200 dark:text-blue-200 menuButton", "block py-2 px-3 rounded-sm lg:p-0"]] "Browse"
        li_ $ a_ [href_ "/settings", classes_ [if current_page == Settings then "text-yellow-500" else "hover:text-blue-200 text-blue-200 dark:text-blue-200 menuButton", "block py-2 px-3 rounded-sm lg:p-0"]] "Settings"
    div_ [class_ "w-full mx-auto pb-2 lg:pt-2"] $ do
      div_ [class_ "px-2"] $ do
        div_ [class_ "max-w-screen-xl w-full mx-auto px-4 flex flex-col md:flex-row place-content-between text-md pt-1 pb-2"] $ do 
          div_ [id_ "currentSong", class_ "text-orange-200 lg:text-lg truncate"] $ maybe (p_ $ toHtmlRaw ("&nbsp;" :: String)) (\song -> do
            p_ [id_ "currentSongTitle"] $ if playbackState /= MPD.Stopped then toHtmlRaw $ title song else ""
            p_ [id_ "currentSongArtist", classes_ (["text-xs"] <> (if showArtistOnNavbar user_config then ["block"] else ["hidden"]))] $ if playbackState /= MPD.Stopped then toHtmlRaw $ artist song else ""
            p_ [id_ "currentSongPath", classes_ (["text-xs"] <> (if showPathOnNavbar user_config then ["block"] else ["hidden"]))] $ if playbackState /= MPD.Stopped then toHtmlRaw $ path song else ""
            ) current_song
          div_ [class_ "flex space-x-4 mt-4 lg:mt-0 text-orange-200 [&_.playerButton]:dark:hover:text-orange-400"] $ do
            button_ [id_ "navPrevious", class_ "playerButton cursor-pointer block"] $ i_ [data_ "feather" "skip-back", class_ "size-6"] ""
            button_ [id_ "navStop", class_ "playerButton cursor-pointer block"] $ i_ [data_ "feather" "square", class_ "size-6"] ""
            button_ [id_ "navPlayPause", class_ "playerButton cursor-pointer block"] $ i_  [data_ "feather" (T.pack playPause_icon), class_ "size-6"] ""
            button_ [id_ "navNext", class_ "playerButton cursor-pointer block"] $ i_ [data_ "feather" "skip-forward", class_ "size-6"] ""
            div_ [class_ "flex items-center w-full"] $ input_ [id_ "navVolume", onchange_ "socket.send('volume,' + this.value)", type_ "range", value_ $ T.pack $ show volume, class_ "w-full h-1 bg-gray-200 rounded-lg appearance-none cursor-pointer dark:bg-orange-200 hover:bg-orange-400"]
        div_ [class_ "max-w-screen-xl flex flex-row items-center justify-between mx-auto px-4 gap-x-4 my-0 lg:my-0 lg:pt-0"] $ do
          div_ [class_ "grow"] $ input_ [id_ "playerProgressInput", oninput_ "socket.send('seekCur,'+this.value)", type_ "range", value_ "0", class_ "focus:outline-none border-none range-lg h-2 w-full rounded-lg cursor-pointer bg-gray-600 dark:bg-slate-800"]
          div_ [class_ "text-orange-200 hidden sm:block"] $ span_ [id_ "elapsedTime", class_ ""] (toHtml elapsed_time) <> span_ [class_ "mx-1"] "/" <> span_ [id_ "totalTime", class_ ""] (toHtml total_time)

------------------------------------------------------------
-- Individual Pages
------------------------------------------------------------

queuePage :: Options -> Maybe UserConfig -> Handler (Html ())
queuePage options user_config = do
  playlist <- liftIO $ withMpdOpt options $ MPD.playlistInfo Nothing
  page options (fromMaybe defaultUserConfig user_config) Queue $ do
    div_ [class_ "flex ml-4 mr-2"] $ do
      span_ [class_ "text-2xl"] $ "Queue"
    div_ [class_ "flex gap-x-1 md:gap-x-6 place-content-between lg:place-content-end ml-2 text-xs md:text-base mt-4 lg:mt-0"] $ do
        button_ [onclick_ "socket.send('consume')", id_ "btnConsume", class_ "py-1 rounded-md text-slate-400 cursor-pointer flex items-center"] (i_ [class_ "size-5 stroke-2", data_ "feather" "file-minus"] "" <> span_ [class_ "ml-1"] "Consume")
        button_ [onclick_ "socket.send('single')", id_ "btnSingle", class_ "py-1 rounded-md text-slate-400 cursor-pointer flex items-center"] (i_ [class_ "size-5 stroke-2", data_ "feather" "star"] "" <> span_ [class_ "ml-1"] "Single")
        button_ [onclick_ "socket.send('random')", id_ "btnRandom", class_ "py-1 rounded-md text-slate-400 cursor-pointer flex items-center"] (i_ [class_ "size-5 stroke-2", data_ "feather" "shuffle"] "" <> span_ [class_ "ml-1"] "Random")
        button_ [onclick_ "socket.send('repeat')", id_ "btnRepeat", class_ "py-1 rounded-md text-slate-400 cursor-pointer flex items-center"] (i_ [class_ "size-5 stroke-2", data_ "feather" "repeat"] "" <> span_ [class_ "ml-1"] "Repeat")
        button_ [onclick_ "socket.send('clear')", class_ "px-2 py-1 rounded-md cursor-pointer flex items-center text-slate-400 hover:text-slate-200"] (i_ [class_ "size-5 stroke-2", data_ "feather" "file"] "" <> span_ [class_ "ml-1"] "Clear")
    case playlist of
      Left _ -> p_ "playlist error"
      Right [] -> div_ [class_ "px-10 mt-4 mb-8 text-rose-400"] $ p_ "-Empty queue-"
      Right pl -> table_ [class_ "table-auto w-full mt-4"] $ do
        tbody_ $ mapM_ (\song ->
                          tr_ [class_ "flex place-content-between px-2 my-0"] $ do
                           td_ [class_ "flex items-center place-content-start w-8"] $ toHtml $ maybe "" (show . (+1)) (MPD.sgIndex song)
                           td_ [class_ "flex place-content-between flex-grow cursor-pointer song-item flex"] $ do
                             button_ ([onclick_ (maybe "alert('Error: No id')" (\x -> "socket.send('playId," <> (T.pack $ show $ unId x) <> "')")  (MPD.sgId song)), class_ "place-content-between py-2 w-full flex flex-row place-content-start focus:outline-none cursor-pointer"] <> ((\songId -> data_ "songId" (T.pack $ show $ (unId songId))) <$> (maybeToList $ MPD.sgId song))) $ do
                               div_ [class_ "grow flex place-content-start justify-content-start text-left"] $ toHtml $ title $ currentSongFromSong song
                               div_ [class_ "px-2"] $ toHtml $ formatTime defaultTimeLocale (if MPD.sgLength song > 3600 then "%H:%M:%S" else "%M:%S") $ posixSecondsToUTCTime $ fromIntegral $ MPD.sgLength song
                           td_ [class_ "flex gap-x-2"] $ do
                             a_ [href_ $ "/browse?path=" <> (U.encodeText $ T.pack $ FP.takeDirectory $ MPD.toString $  MPD.sgFilePath song), class_ "pl-0 pr-4 w-1 hover:text-blue-600 text-blue-300 cursor-pointer my-auto"] $ i_ [class_ "size-4 stroke-3", data_ "feather" "search"] ""
                             button_ [onclick_ (maybe "alert('Error: No id')" (\x -> "socket.send('deleteId," <> (T.pack $ show $ unId x) <> "')")  (MPD.sgId song)), class_ "pl-0 pr-4 w-1 text-red-300 hover:text-red-400 dark:text-rose-300 cursor-pointer"] $ i_ [class_ "size-4 stroke-2", data_ "feather" "trash-2"] ""
                       ) pl


browsePage :: Options -> Maybe UserConfig -> Maybe String -> Handler (Html ())
browsePage options user_config query_path = do
  let dirlist = FP.splitDirectories $ maybe "" (fromString . id) query_path
  mpdResult <- liftIO $ withMpdOpt options $ MPD.lsInfo $ maybe "" (fromString . id) query_path
  page options (fromMaybe defaultUserConfig user_config) Browse $ do
    div_ [class_ "flex flex-col md:flex-row place-content-between ml-4 mr-2"] $ do
      div_ [class_ "place-content-start"] $ do
        span_ [class_ "text-2xl"] $ "Browse"
        mapM_ (\(path, padding, dir, index) -> 
                    if index < length dirlist then p_ [class_ "text-sm truncate"] $ (toHtmlRaw padding) <>  (a_ [class_ "text-blue-400 dark:text-slate-400 hover:dark:text-blue-400", href_ $ "/browse?path=" <> (U.encodeText $ T.pack path)] $ toHtmlRaw $ if index > 1 then ("&#10551;&nbsp;" <> dir) else dir)
                    else p_ [class_ "text-sm"] $ (toHtmlRaw padding) <> (span_ [class_ "text-lime-400"] $ toHtmlRaw $ if index > 1 then ("&#10551;&nbsp;" <> dir) else dir)
              ) $ zip4 (reverse $ mkPathList $ dirlist) (mkPaddingList "&nbsp;" dirlist) (dirlist)
          [1 .. length dirlist]
      case query_path of
        Just (_x:_xs) -> do
          div_ [class_ "mt-4 md:mt-4 lg:items-end items-center flex place-content-around lg:place-content-end md:gap-x-6 [&_button]:text-slate-400 [&_button]:hover:text-slate-200"] $ do
            button_ [onclick_ $ "location.href='/browse" <> (if length dirlist > 1 then T.pack $ "?path=" <> (mconcat $ intersperse "/" (init dirlist)) else "") <> "'", class_ "rounded-md text-white cursor-pointer flex items-center"] $ (i_ [class_ "size-5 stroke-3", data_ "feather" "corner-left-up"] "" <> span_ [class_ "ml-1"] "Up")
            button_ [onclick_ $ "location.href='/browse'", class_ "rounded-md text-white cursor-pointer flex items-center"] $ (i_ [class_ "size-5 stroke-3", data_ "feather" "chevrons-up"] "" <> span_ [class_ "ml-1"] "Top")
            button_ [onclick_ "socket.send(\"update,\"+new URLSearchParams(window.location.search).get('path'))", class_ "rounded-md text-white cursor-pointer flex items-center"] $ (i_ [class_ "size-4 stroke-2", data_ "feather" "refresh-ccw"] "" <> span_ [class_ "ml-1"] "Update")
            button_ [onclick_ "socket.send(\"addPath,\"+new URLSearchParams(window.location.search).get('path'))", class_ "rounded-md text-white cursor-pointer flex items-center"] $ (i_ [class_ "size-5 stroke-3", data_ "feather" "plus"] "" <> span_ [class_ "ml-1"] "Add all")
            button_ [onclick_ "socket.send(\"playPath,\"+new URLSearchParams(window.location.search).get('path'))", class_ "rounded-md text-white cursor-pointer flex items-center"] $ (i_ [class_ "size-5 stroke-2", data_ "feather" "play"] "" <> span_ [class_ "ml-1"] "Play all")
        _ -> div_ ""
    case mpdResult of
      Left e -> p_ "Browse error" <> p_ (toHtml $ show e)
      Right res -> table_ [class_ "table-auto w-full mt-2 lg:mt-4"] $ do
        tbody_ $ mapM_ (\item -> tr_ [class_ "hover:bg-sky-100 flex place-content-between px-2 my-0"] $ do
                           case item of
                             MPD.LsDirectory path -> do
                               mkIconField "folder"
                               td_ [class_ "pl-4 py-0 overflow-hidden grow cursor-pointer flex"] $ do
                                 a_ [href_ (("/browse?path=" :: T.Text) <> (U.encodeText $ MPD.toText path)), class_ "py-2 grow w-full h-full"] $ toHtml $ fromMaybe "__" ((FP.splitDirectories $ MPD.toString path) !!? (( length $ FP.splitDirectories $ MPD.toString path ) - 1))
                               mkQueueButtons(T.pack $ MPD.toString path)
                             MPD.LsSong song -> do
                               mkIconField "music"
                               td_ [class_ "pl-4 py-2 overflow-hidden grow flex place-content-between"] $ do
                                 div_ $ toHtml $ maybe (FP.takeBaseName $ MPD.toString $ MPD.sgFilePath song) (\x -> maybe "No title metadata" (FP.takeFileName . MPD.toString) (listToMaybe x)) (C.lookup MPD.Title (MPD.sgTags song))
                                 div_ [class_ "px-4"] $ toHtml $ formatTime defaultTimeLocale (if MPD.sgLength song > 3600 then "%H:%M:%S" else "%M:%S") $ posixSecondsToUTCTime $ fromIntegral $ MPD.sgLength song
                               mkQueueButtons(T.pack $ MPD.toString $ MPD.sgFilePath song)
                             MPD.LsPlaylist playlist -> do 
                               mkIconField "list"
                               td_ [class_ "pl-4 py-2 overflow-hidden grow"] $ toHtml $ FP.takeFileName $ MPD.toString playlist
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
    mkPathList :: [String] -> [String]
    mkPathList [] = []
    mkPathList xs = [mconcat $ intersperse "/" xs] ++ (mkPathList $ init xs) 
    mkPaddingList :: String -> [String] -> [String]
    mkPaddingList _ [] = []
    mkPaddingList padding (_:xs) = [padding] <> (mkPaddingList (padding <> "&nbsp;&nbsp;&nbsp;&nbsp;") xs)
    mkIconField :: T.Text -> Html ()
    mkIconField icon = td_ [class_ "text-slate-400 flex items-center"] $ i_ [class_ "size-4", data_ "feather" icon] ""
    mkQueueButtons :: T.Text -> Html ()
    mkQueueButtons path = td_ [class_ "py-2 [&_button]:text-blue-400 [&_button]:hover:text-cyan-950 flex gap-x-2 md:gap-x-2"] $ do 
      button_ [onclick_ $ "socket.send('addPath," <> path <> "')", class_ "cursor-pointer"] $ i_ [class_ "size-5 stroke-3", data_ "feather" "plus"] "__"
      button_ [onclick_ $ "socket.send('playPath," <> path <> "')", class_ "cursor-pointer"] $ i_ [class_ "size-5 stroke-3", data_ "feather" "play"] "__"

settingsPage :: Options -> Maybe UserConfig -> Handler (Html ())
settingsPage options user_config = do
  page options (fromMaybe defaultUserConfig user_config) Settings $ do
    p_ [class_ "ml-4 text-2xl"] "Settings"
    div_ [class_ "mx-4 md:ml-8 md:mr-20 mpb-4"] $ do
      button_ [id_ "updateAll", class_ "bg-blue-500 hover:bg-blue-600 py-2 px-4 my-4 rounded text-white flex items-center gap-x-1"] $ "Update DB"
      div_ [class_ "mt-8 bg-slate-600 text-slate-300 rounded p-8 md:min-w-3/4 flex flex-col gap-y-4 justify-items-start w-fit "] $ do
        p_ $ do
          input_ [id_ "showArtistOnNavbar", type_ "checkbox", class_ "mr-2"]
          label_ [class_ "", for_ "showArtistOnNavbar"] $ "Show artist name on nav bar player"
        p_ $ do
          input_ [id_ "showPathOnNavbar", type_ "checkbox", class_ "mr-2"]
          label_ [class_ "", for_ "showPathOnNavbar"] $ "Show filepath on nav bar player"
