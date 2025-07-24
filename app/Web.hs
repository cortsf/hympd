{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
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
import Data.List (sortBy, intersperse)
import Data.Coerce (coerce)
import Lucid
import Servant (Handler)
import GHC.Generics
-- import Data.Char (chr)
import qualified Data.Text as T
import qualified Network.MPD as MPD
import qualified Data.Map.Strict as C
import qualified System.FilePath.Posix as FP

------------------------------------------------------------
-- Common
------------------------------------------------------------

data Page = Queue | Browse | Settings deriving Eq

page :: Page -> Html () -> Handler (Html ())
page current_page content = do
  pure $ html_ $ do
    head_ $ do
      title_ "Hympd"
      script_ [src_ "static/styles.css"] ("" :: String)
      script_ [src_ "static/icons.js"] ("" :: String)
      link_ [rel_ "icon", href_ "static/favicon4.png", sizes_ "any", type_ "image/png"]
    body_ [class_ "overflow-y-scroll flex flex-col h-screen bg-blue-200 dark:bg-gray-900 focus:outline-none dark:text-slate-400"] $ do
      nav current_page
      div_ [id_ "content", class_ "max-w-screen-xl w-full flex flex-wrap flex-col flex-grow mx-auto pt-18 pb-20 bg-white dark:bg-slate-800 [&_tr]:odd:bg-slate-50 [&_tr]:odd:dark:bg-slate-700 [&_tr]:even:bg-white [&_tr]:even:dark:bg-slate-800 [&_tr]:dark:hover:bg-stone-200 [&_tr]:dark:hover:text-blue-500"] $ do
        content
      footer
      script_ $ "feather.replace();"
      script_ $ jsblock

nav :: Page -> Html ()
nav current_page = nav_ [class_ "bg-gray-900 dark:bg-slate-700 fixed w-full dark:text-blue-200  [&_.navItem]:dark:hover:text-yellow-600 border-none"] $ do
  div_ [class_ "max-w-screen-xl flex flex-wrap items-center justify-between mx-auto p-4"] $ do
    div_ [class_ "hidden w-full md:block md:w-auto", id_ "navbar-default"] $ do
      ul_ [class_ "font-medium flex flex-row space-x-8"] $ do
        li_ $ a_ [href_ "/queue", classes_ [if current_page == Queue then "text-yellow-500" else "hover:text-blue-200 text-blue-500 dark:text-blue-200 navItem", "block py-2 px-3 bg-blue-700 rounded-sm md:bg-transparent md:p-0"]] "Queue"
        li_ $ a_ [href_ "/browse", classes_ [if current_page == Browse then "text-yellow-500" else "hover:text-blue-200 text-blue-500 dark:text-blue-200 navItem", "block py-2 px-3 bg-blue-700 rounded-sm md:bg-transparent md:p-0"]] "Browse"
        li_ $ a_ [href_ "/settings", classes_ [if current_page == Settings then "text-yellow-500" else "hover:text-blue-200 text-blue-500 dark:text-blue-200 navItem", "block py-2 px-3 bg-blue-700 rounded-sm md:bg-transparent md:p-0"]] "Settings"
    div_ [class_ "flex space-x-4 "] $ do
      button_ [id_ "navPrevious", class_ "navItem cursor-pointer block bg-blue-200 rounded-sm md:bg-transparent hover:text-blue-200"] $ i_ [data_ "feather" "skip-back"] ""
      button_ [id_ "navStop", class_ "navItem cursor-pointer block bg-blue-200 rounded-sm md:bg-transparent hover:text-blue-200"] $ i_ [data_ "feather" "square"] ""
      button_ [id_ "navPlayPause", class_ "navItem cursor-pointer block bg-blue-200 rounded-sm md:bg-transparent hover:text-blue-200"] $ i_  [data_ "feather" "play"] ""
      button_ [id_ "navNext", class_ "navItem cursor-pointer block bg-blue-200 rounded-sm md:bg-transparent hover:text-blue-200"] $ i_ [data_ "feather" "skip-forward"] ""
      div_ [class_ "flex items-center"] $ input_ [id_ "navVolume", onchange_ "socket.send('volume,' + this.value)", type_ "range", value_ "0", class_ "w-full h-1 bg-gray-200 rounded-lg appearance-none cursor-pointer dark:bg-blue-200"]

footer :: Html ()
footer = div_ [class_ "bg-slate-600 dark:bg-slate-600 fixed bottom-0 w-full py-4"] $ do
  div_ [class_ "max-w-screen-xl w-full mx-auto"] $ do 
    h1_ [id_ "currentSong", class_ "text-2xl text-blue-200"] $ ".."
  div_ [class_ "max-w-screen-xl flex items-center justify-between mx-auto"] $ do
    input_ [id_ "footerProgress", oninput_ "socket.send('seekCur,'+this.value)", type_ "range", value_ "0", class_ "mt-2 w-full h-1 bg-gray-200 rounded-lg cursor-pointer dark:bg-gray-700"]


------------------------------------------------------------
-- Individual Pages
------------------------------------------------------------

queuePage :: Handler (Html ())
queuePage = do
  playlist <- liftIO $ MPD.withMPD $ MPD.playlistInfo Nothing
  page Queue $ do
    div_ [class_ "flex place-content-between ml-10 mr-2 [&_button]:dark:bg-blue-400 [&_button]:dark:hover:bg-blue-300"] $ do
      span_ [class_ "text-2xl"] $ "Queue"
      div_ [class_ "flex gap-x-2"]$ do
        button_ [onclick_ "alert('Not implemented')", class_ "px-2 py-1 rounded-md text-white cursor-pointer flex items-center"] (i_ [class_ "size-5 stroke-2", data_ "feather" "shuffle"] "" <> span_ [class_ "ml-1"] "Random")
        button_ [onclick_ "alert('Not implemented')", class_ "px-2 py-1 rounded-md text-white cursor-pointer flex items-center"] (i_ [class_ "size-5 stroke-2", data_ "feather" "file-minus"] "" <> span_ [class_ "ml-1"] "Consume")
        button_ [onclick_ "alert('Not implemented')", class_ "px-2 py-1 rounded-md text-white cursor-pointer flex items-center"] (i_ [class_ "size-5 stroke-2", data_ "feather" "star"] "" <> span_ [class_ "ml-1"] "Single")
        button_ [onclick_ "alert('Not implemented')", class_ "px-2 py-1 rounded-md text-white cursor-pointer flex items-center"] (i_ [class_ "size-5 stroke-2", data_ "feather" "repeat"] "" <> span_ [class_ "ml-1"] "Repeat")
        button_ [onclick_ "socket.send('clear')", class_ "px-2 py-1 rounded-md text-white cursor-pointer flex items-center"] (i_ [class_ "size-5 stroke-2", data_ "feather" "file"] "" <> span_ [class_ "ml-1"] "Clear")
    case playlist of
      Left _ -> p_ "playlist error"
      Right [] -> div_ [class_ "px-10 mt-4 text-rose-400"] $ p_ "-Empty queue-"
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
  mpdResult <- liftIO $ MPD.withMPD $ MPD.lsInfo $ maybe "" (fromString . id) query_path
  page Browse $ do
    div_ [class_ "flex place-content-between ml-10 mr-2"] $ do
      div_ [class_ "place-content-start"] $ do
        span_ [class_ "text-2xl"] $ a_ [href_ "/browse", class_ "hover:text-blue-300"] "Browse"
        mapM_ (\(path, dir) -> p_ [class_ "text-xs"] $ a_ [class_ "hover:text-blue-300", href_ $ "/browse?path=" <> T.pack path] $ toHtml dir) $ zip (reverse $ combine $ FP.splitDirectories $ maybe "" (fromString . id) query_path) (FP.splitDirectories $ maybe "" (fromString . id) query_path)
      if (isJust query_path) then div_ [class_ "flex gap-x-2 [&_button]:dark:bg-blue-400 [&_button]:dark:hover:bg-blue-300 items-end"] $ do
        button_ [onclick_ "socket.send(\"addPath,\"+new URLSearchParams(window.location.search).get('path'))", class_ "px-2 py-1 rounded-md text-white cursor-pointer flex items-center"] (i_ [class_ "size-5 stroke-3", data_ "feather" "plus"] "" <> span_ [class_ "ml-1"] "Add all")
        button_ [onclick_ "socket.send(\"playPath,\"+new URLSearchParams(window.location.search).get('path'))", class_ "px-2 py-1 rounded-md text-white cursor-pointer flex items-center"] $ (i_ [class_ "size-5 stroke-2", data_ "feather" "play"] "" <> span_ [class_ "ml-1"] "Play all")
        else  div_ ""
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
    combine :: [String] -> [String]
    combine [] = []
    combine xs = [mconcat $ intersperse "/" xs] ++ (combine $ init xs) 
    mkIconField :: T.Text -> Html ()
    mkIconField icon = td_ [class_ "text-slate-400 flex items-center"] $ i_ [class_ "size-4", data_ "feather" icon] ""
    mkQueueButtons :: T.Text -> Html ()
    mkQueueButtons path = td_ [class_ "py-2 dark:text-blue-400 flex gap-x-4"] $ do 
      button_ [onclick_ $ "socket.send('addPath," <> path <> "')", class_ "hover:text-cyan-900 cursor-pointer"] $ i_ [class_ "size-5 stroke-3", data_ "feather" "plus"] "__"
      button_ [onclick_ $ "socket.send('playPath," <> path <> "')", class_ "hover:text-cyan-900 cursor-pointer"] $ i_ [class_ "size-5 stroke-3", data_ "feather" "play"] "__"
-- parentDirs :: FilePath -> [(T.Text, T.Text)]
-- parentDirs path = (\(fst, snd) -> foldr ()  tail_path)
--   let currentPath = FP.joinPath [path]
--   let parents = map (`FP.joinPath` []) $ filter (/= "") $ tail $ FP.splitDirectories currentPath
--   return parents

settingsPage :: Handler (Html ())
settingsPage = do
  mpdResult <- liftIO $ MPD.withMPD $ MPD.config
  mpdStatus <- liftIO $ MPD.withMPD $ MPD.status
  page Settings $ do
    p_ [class_ "text-2xl ml-10"] "Settings"
    div_ [class_ "bg-gray-200 mt-4 p-4"] $ toHtml $ show mpdStatus
    div_ [class_ "bg-blue-200 mt-4 w-full"] $ toHtml $ show mpdResult
