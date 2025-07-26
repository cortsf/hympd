{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
module Web where

import Javascript
import Utility

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
import Data.Text qualified as T
import Text.Pretty.Simple qualified as P
import Network.MPD qualified as MPD
import Data.Map.Strict qualified as C
import System.FilePath.Posix qualified as FP

------------------------------------------------------------
-- Common
------------------------------------------------------------

data CurrentPage = Queue | Browse | Settings deriving Eq

page :: CurrentPage -> Html () -> Handler (Html ())
page current_page content = do
  mpdStatus <- liftIO $ MPD.withMPD $ MPD.status
  let current_time = case mpdStatus of
        Left _ -> "0"
        Right status -> case MPD.stTime status of
          Nothing -> "0"
          Just time -> T.pack $ show $ ((fst time / snd time) * 100)
  pure $ html_ $ do
    head_ $ do
      title_ "Hympd"
      script_ [src_ "static/styles.css"] ("" :: String)
      script_ [src_ "static/icons.js"] ("" :: String)
      link_ [rel_ "icon", href_ "static/favicon4.png", sizes_ "any", type_ "image/png"]
      -- link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/flowbite@3.1.2/dist/flowbite.min.css"]
      style_ $ T.pack "#playerProgressInput{-webkit-appearance: none; background: oklch(55.6% 0 0); background-image: linear-gradient(#FFD6A8, #FFD6A8); background-size: " <> current_time <> "% 100%; background-repeat: no-repeat;}#playerProgressInput::-webkit-slider-thumb {-webkit-appearance: none; height: 0px; width: 0px;}"
    body_ [class_ "overflow-y-scroll flex flex-col bg-blue-200 dark:bg-gray-900 focus:outline-none dark:text-slate-400"] $ do
      nav_full current_page
      div_ [id_ "content", class_ "overflow-y-visible max-w-screen-xl w-full grow flex flex-col mx-auto pt-4 bg-white dark:bg-slate-800 [&_tr]:odd:bg-slate-50 [&_tr]:odd:dark:bg-slate-700 [&_tr]:even:bg-white [&_tr]:even:dark:bg-slate-800 [&_tr]:dark:hover:bg-sky-900"] $ do
        content
      script_ $ "feather.replace();"
      script_ $ jsblock
      script_ [src_ "https://cdn.jsdelivr.net/npm/flowbite@3.1.2/dist/flowbite.min.js"] ("" :: String)

nav_full :: CurrentPage -> Html ()
nav_full current_page = nav_ [class_ "sticky top-0 w-full dark:text-blue-200"] $ do
  div_ [class_ "bg-gray-900 dark:bg-slate-700 w-full [&_.menuButton]:dark:hover:text-yellow-500"] $ do
    div_ [class_ "max-w-screen-xl w-full flex flex-row place-content-between mx-auto pt-4 px-4"] $ do
      div_ [class_ "hidden w-full md:block md:w-auto", id_ "navbar-default"] $ do
        ul_ [class_ "font-medium flex flex-row space-x-8"] $ do
          li_ $ a_ [href_ "/queue", classes_ [if current_page == Queue then "text-yellow-500" else "hover:text-blue-200 text-blue-500 dark:text-blue-200 menuButton", "block py-2 px-3 bg-blue-700 rounded-sm md:bg-transparent md:p-0"]] "Queue"
          li_ $ a_ [href_ "/browse", classes_ [if current_page == Browse then "text-yellow-500" else "hover:text-blue-200 text-blue-500 dark:text-blue-200 menuButton", "block py-2 px-3 bg-blue-700 rounded-sm md:bg-transparent md:p-0"]] "Browse"
          li_ $ a_ [href_ "/settings", classes_ [if current_page == Settings then "text-yellow-500" else "hover:text-blue-200 text-blue-500 dark:text-blue-200 menuButton", "block py-2 px-3 bg-blue-700 rounded-sm md:bg-transparent md:p-0"]] "Settings"
  div_ [class_ "bg-slate-600 dark:bg-slate-700 w-full mx-auto pb-2 pt-2"] $ do
    div_ [class_ "px-8"] $ do
      div_ [class_ "max-w-screen-xl w-full mx-auto px-4 flex place-content-between text-md pt-1 pb-2 text-lg"] $ do 
        span_ [id_ "currentSong", class_ "text-orange-200 mr-4"] $ "Song Title, a very long one, you can see!"
        div_ [class_ "flex space-x-4 text-orange-200 [&_.playerButton]:dark:hover:text-orange-400"] $ do
          button_ [id_ "navPrevious", class_ "playerButton cursor-pointer block"] $ i_ [data_ "feather" "skip-back", class_ "size-6"] ""
          button_ [id_ "navStop", class_ "playerButton cursor-pointer block"] $ i_ [data_ "feather" "square", class_ "size-6"] ""
          button_ [id_ "navPlayPause", class_ "playerButton cursor-pointer block"] $ i_  [data_ "feather" "play", class_ "size-6"] ""
          button_ [id_ "navNext", class_ "playerButton cursor-pointer block"] $ i_ [data_ "feather" "skip-forward", class_ "size-6"] ""
          div_ [class_ "flex items-center"] $ input_ [id_ "navVolume", onchange_ "socket.send('volume,' + this.value)", type_ "range", value_ "0", class_ "w-full h-1 bg-gray-200 rounded-lg appearance-none cursor-pointer dark:bg-orange-200 hover:bg-orange-400"]
      div_ [class_ "max-w-screen-xl flex flex-row items-center justify-between mx-auto px-4 gap-x-4 mt-0 pt-0"] $ do
        div_ [class_ "grow"] $ input_ [id_ "playerProgressInput", oninput_ "socket.send('seekCur,'+this.value)", type_ "range", value_ "0", class_ "focus:outline-none border-none range-lg h-2 w-full h-1 rounded-lg cursor-pointer bg-gray-600 dark:bg-slate-800"]
        div_ [class_ "text-orange-200"] $ span_ [id_ "elapsedTime", class_ ""] "00:00" <> span_ [class_ "mx-1"] "/" <> span_ [id_ "totalTime", class_ ""] "00:00"

nav_compact :: CurrentPage -> Html ()
nav_compact current_page = nav_ [class_ "sticky top-0 w-full bg-gray-900 dark:bg-gray-700 dark:text-blue-200 [&_.navItem]:dark:hover:text-yellow-600"] $ do
  div_ [class_ "max-w-screen-xl w-full flex flex-row mx-auto"] $ do
    div_ [class_ "flex mx-auto pt-4 px-4 h-full mr-20"] $ do
        div_ [class_ "hidden w-full md:block md:w-auto", id_ "navbar-default"] $ do
          ul_ [class_ "font-medium flex flex-row space-x-8"] $ do
            li_ $ a_ [href_ "/queue", classes_ [if current_page == Queue then "text-yellow-500" else "hover:text-blue-200 text-blue-500 dark:text-blue-200 navItem", "block py-2 px-3 bg-blue-700 rounded-sm md:bg-transparent md:p-0"]] "Queue"
            li_ $ a_ [href_ "/browse", classes_ [if current_page == Browse then "text-yellow-500" else "hover:text-blue-200 text-blue-500 dark:text-blue-200 navItem", "block py-2 px-3 bg-blue-700 rounded-sm md:bg-transparent md:p-0"]] "Browse"
            li_ $ a_ [href_ "/settings", classes_ [if current_page == Settings then "text-yellow-500" else "hover:text-blue-200 text-blue-500 dark:text-blue-200 navItem", "block py-2 px-3 bg-blue-700 rounded-sm md:bg-transparent md:p-0"]] "Settings"
    div_ [class_ "w-full mx-auto pt-2 pb-4 px-4 flex flex-col"] $ do
      div_ [class_ "flex space-x-4 "] $ do
        div_ [class_ "max-w-screen-xl w-full mx-auto"] $ do 
          p_ [id_ "currentSong", class_ "text-md text-blue-200"] $ "Song Title"
        button_ [id_ "navPrevious", class_ "navItem cursor-pointer block bg-blue-200 rounded-sm md:bg-transparent hover:text-blue-200"] $ i_ [data_ "feather" "skip-back"] ""
        button_ [id_ "navStop", class_ "navItem cursor-pointer block bg-blue-200 rounded-sm md:bg-transparent hover:text-blue-200"] $ i_ [data_ "feather" "square"] ""
        button_ [id_ "navPlayPause", class_ "navItem cursor-pointer block bg-blue-200 rounded-sm md:bg-transparent hover:text-blue-200 focus:text-blue-200"] $ i_  [data_ "feather" "play"] ""
        button_ [id_ "navNext", class_ "navItem cursor-pointer block bg-blue-200 rounded-sm md:bg-transparent hover:text-blue-200"] $ i_ [data_ "feather" "skip-forward"] ""
        div_ [class_ "flex items-center"] $ input_ [id_ "navVolume", onchange_ "socket.send('volume,' + this.value)", type_ "range", value_ "0", class_ "w-full h-1 bg-gray-200 rounded-lg appearance-none cursor-pointer dark:bg-blue-200"]
      div_ [class_ "max-w-screen-xl w-full flex items-center justify-between mx-auto mt-2"] $ do
          input_ [id_ "playerProgressInput", oninput_ "socket.send('seekCur,'+this.value)", type_ "range", value_ "0", class_ "h-2 mt-1 w-full h-1 bg-gray-600 rounded-lg cursor-pointer dark:bg-slate-800"]


------------------------------------------------------------
-- Individual Pages
------------------------------------------------------------

queuePage :: Handler (Html ())
queuePage = do
  playlist <- liftIO $ MPD.withMPD $ MPD.playlistInfo Nothing
  page Queue $ do
    div_ [class_ "flex place-content-between ml-4 mr-2"] $ do
      span_ [class_ "text-2xl"] $ "Queue"
      div_ [class_ "flex gap-x-2"] $ do
        button_ [onclick_ "socket.send('consume')", id_ "btnConsume", data_ "tooltip-target" "consume_tooltip", data_ "tooltip-placement" "bottom", class_ "px-2 py-1 rounded-md text-slate-400 cursor-pointer flex items-center"] (i_ [class_ "size-5 stroke-2", data_ "feather" "file-minus"] "" <> span_ [class_ "ml-1"] "Consume")
        button_ [onclick_ "socket.send('single')", id_ "btnSingle", data_ "tooltip-target" "single_tooltip", data_ "tooltip-placement" "bottom", class_ "px-2 py-1 rounded-md text-slate-400 cursor-pointer flex items-center"] (i_ [class_ "size-5 stroke-2", data_ "feather" "star"] "" <> span_ [class_ "ml-1"] "Single")
        button_ [onclick_ "socket.send('random')", id_ "btnRandom", class_ "px-2 py-1 rounded-md text-slate-400 cursor-pointer flex items-center"] (i_ [class_ "size-5 stroke-2", data_ "feather" "shuffle"] "" <> span_ [class_ "ml-1"] "Random")
        button_ [onclick_ "socket.send('repeat')", id_ "btnRepeat", class_ "px-2 py-1 rounded-md text-slate-400 cursor-pointer flex items-center"] (i_ [class_ "size-5 stroke-2", data_ "feather" "repeat"] "" <> span_ [class_ "ml-1"] "Repeat")
        button_ [onclick_ "socket.send('clear')", class_ "px-2 py-1 rounded-md cursor-pointer flex items-center text-slate-400 hover:text-slate-200"] (i_ [class_ "size-5 stroke-2", data_ "feather" "file"] "" <> span_ [class_ "ml-1"] "Clear")
        div_ [id_ "consume_tooltip", role_ "tooltip", class_ "absolute z-10 invisible inline-block px-3 py-1 text-sm font-medium text-white transition-opacity duration-200 bg-gray-900 rounded-lg shadow-xs opacity-0 tooltip dark:bg-slate-400"] $ do
          (toHtml $ T.pack "When consume is activated, each song played is removed from playlist.")
        div_ [id_ "single_tooltip", role_ "tooltip", class_ "absolute z-10 invisible inline-block px-3 py-1 text-sm font-medium text-white transition-opacity duration-300 bg-gray-900 rounded-lg shadow-xs opacity-0 tooltip dark:bg-slate-400"] $ do
          (toHtml $ T.pack "When single is activated, playback is stopped after current song, or song is repeated if the ‘repeat’ mode is enabled.")
    case playlist of
      Left _ -> p_ "playlist error"
      Right [] -> div_ [class_ "px-10 mt-4 mb-8 text-rose-400"] $ p_ "-Empty queue-"
      Right pl -> table_ [class_ "table-auto w-full mt-4"] $ do
        tbody_ $ mapM_ (\song ->
                          tr_ [class_ "flex place-content-between px-2 my-0"] $ do
                           td_ [class_ "flex items-center place-content-start w-8"] $ toHtml $ maybe "" (show . (+1)) (MPD.sgIndex song)
                           td_ [class_ "flex place-content-between flex-grow cursor-pointer song-item flex"] $ do
                             button_ ([onclick_ (maybe "alert('Error: No id')" (\x -> "socket.send('playId," <> (T.pack $ show $ unId x) <> "')")  (MPD.sgId song)), class_ "place-content-between py-2 w-full flex flex-row place-content-start focus:outline-none cursor-pointer"] <> ((\songId -> data_ "songId" (T.pack $ show $ (unId songId))) <$> (maybeToList $ MPD.sgId song))) $ do
                               div_ [class_ "text-ellipsis grow flex place-content-start"] $ toHtml $ maybe (FP.takeBaseName $ MPD.toString $ MPD.sgFilePath song) (\x -> maybe "No title metadata" (MPD.toString) (listToMaybe x)) (C.lookup MPD.Title (MPD.sgTags song))
                               div_ [class_ "px-2"] $ toHtml $ formatTime defaultTimeLocale (if MPD.sgLength song > 3600 then "%H:%M:%S" else "%M:%S") $ posixSecondsToUTCTime $ fromIntegral $ MPD.sgLength song
                           td_ [class_ "flex gap-x-2"] $ do
                             button_ [onclick_ $ "location.href='/browse?path=" <> (T.pack $ FP.takeDirectory $ MPD.toString $  MPD.sgFilePath song) <> "'", class_ "pl-0 pr-4 w-1 text-red-300 hover:text-red-400 dark:text-rose-300 cursor-pointer"] $ i_ [class_ "size-4 stroke-2", data_ "feather" "search"] ""
                             button_ [onclick_ (maybe "alert('Error: No id')" (\x -> "socket.send('deleteId," <> (T.pack $ show $ unId x) <> "')")  (MPD.sgId song)), class_ "pl-0 pr-4 w-1 text-red-300 hover:text-red-400 dark:text-rose-300 cursor-pointer"] $ i_ [class_ "size-4 stroke-2", data_ "feather" "trash-2"] ""
                       ) pl


browsePage :: Maybe String -> Handler (Html ())
browsePage query_path = do
  let dirlist = FP.splitDirectories $ maybe "" (fromString . id) query_path
  mpdResult <- liftIO $ MPD.withMPD $ MPD.lsInfo $ maybe "" (fromString . id) query_path
  page Browse $ do
    div_ [class_ "flex place-content-between ml-4 mr-2"] $ do
      div_ [class_ "place-content-start"] $ do
        span_ [class_ "text-2xl"] $ if isJust query_path then a_ [href_ "/browse", class_ "hover:text-blue-300"] "Browse" else "Browse"
        mapM_ (\(path, padding, dir, index) -> 
                 p_ [class_ "text-sm"] $ (toHtmlRaw padding) >> (if index > 1 then (toHtmlRaw $ T.pack ("&#11169;&nbsp;")) else "") >>
                    if index < length dirlist then (a_ [class_ "hover:text-blue-400", href_ $ "/browse?path=" <> T.pack path] $ toHtml dir)
                    else span_ [class_ "text-lime-400"] $ toHtmlRaw dir
              ) $ zip4 (reverse $ mkPathList $ dirlist) (mkPaddingList "&nbsp;" dirlist) (dirlist)
          [1 .. length dirlist]
      case query_path of
        Just (_x:_xs) -> do
          div_ [class_ "items-end flex gap-x-2 [&_button]:text-slate-400 [&_button]:hover:text-slate-200"] $ do
            button_ [onclick_ $ "location.href='/browse" <> (if length dirlist > 1 then T.pack $ "?path=" <> (mconcat $ intersperse "/" (init dirlist)) else "") <> "'", class_ "px-2 py-1 rounded-md text-white cursor-pointer flex items-center"] $ (i_ [class_ "size-5 stroke-3", data_ "feather" "corner-left-up"] "" <> span_ [class_ "ml-1"] "Up")
            button_ [onclick_ "socket.send(\"addPath,\"+new URLSearchParams(window.location.search).get('path'))", class_ "px-2 py-1 rounded-md text-white cursor-pointer flex items-center"] $ (i_ [class_ "size-5 stroke-3", data_ "feather" "plus"] "" <> span_ [class_ "ml-1"] "Add all")
            button_ [onclick_ "socket.send(\"playPath,\"+new URLSearchParams(window.location.search).get('path'))", class_ "px-2 py-1 rounded-md text-white cursor-pointer flex items-center"] $ (i_ [class_ "size-5 stroke-2", data_ "feather" "play"] "" <> span_ [class_ "ml-1"] "Play all")
        _ -> div_ ""
    case mpdResult of
      Left e -> p_ "Browse error" <> p_ (toHtml $ show e)
      Right res -> table_ [class_ "table-auto w-full mt-4"] $ do
        tbody_ $ mapM_ (\item -> tr_ [class_ "hover:bg-sky-100 flex place-content-between px-2 my-0"] $ do
                           case item of
                             MPD.LsDirectory path -> do
                               mkIconField "folder"
                               td_ [class_ "pl-4 py-0 overflow-hidden grow cursor-pointer flex"] $ do
                                 a_ [href_ (("/browse?path=" :: T.Text) <> (MPD.toText path)), class_ "py-2 grow w-full h-full"] $ toHtml $ fromMaybe "__" ((FP.splitDirectories $ MPD.toString path) !!? (( length $ FP.splitDirectories $ MPD.toString path ) - 1))
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
    mkQueueButtons path = td_ [class_ "py-2 [&_button]:dark:text-blue-400 [&_button]:dark:hover:text-cyan-950 flex gap-x-4"] $ do 
      button_ [onclick_ $ "socket.send('addPath," <> path <> "')", class_ "cursor-pointer"] $ i_ [class_ "size-5 stroke-3", data_ "feather" "plus"] "__"
      button_ [onclick_ $ "socket.send('playPath," <> path <> "')", class_ "cursor-pointer"] $ i_ [class_ "size-5 stroke-3", data_ "feather" "play"] "__"

settingsPage :: Handler (Html ())
settingsPage = do
  mpdResult <- liftIO $ MPD.withMPD $ MPD.config
  mpdStatus <- liftIO $ MPD.withMPD $ MPD.status
  page Settings $ do
    p_ [class_ "text-2xl ml-4"] "Settings"
    -- div_ [class_ "bg-gray-200 mt-4 p-4"] $ pre_ $ toHtml $ P.pShowNoColor mpdStatus
    -- div_ [class_ "bg-gray-200 mt-4 p-4"] $ fromEither $ (pre_ . toHtml . P.pShowNoColor ) <$> mpdStatus
    div_ [class_ "bg-gray-200 mt-4 p-4"] $ pre_ $ toHtml $ either P.pShowNoColor P.pShowNoColor mpdStatus
    div_ [class_ "bg-blue-200 mt-4 w-full"] $ toHtml $ show mpdResult
