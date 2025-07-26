{-# OPTIONS_GHC -fno-warn-orphans #-}
module Utility where
import Data.Maybe (listToMaybe)
import Network.MPD qualified as MPD
import Data.Aeson qualified as A 
import GHC.Generics qualified as G
import Data.Text qualified as T
import System.FilePath.Posix qualified as FP
import Data.Map.Strict qualified as C

data SongList = SongList { songId :: Int, songName :: String } deriving G.Generic
instance A.ToJSON SongList

unId :: MPD.Id -> Int
unId (MPD.Id n) = n

infix 9 !!?
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
    | i < 0     = Nothing
    | otherwise = go i xs
  where
    go :: Int -> [a] -> Maybe a
    go 0 (x:_)  = Just x
    go j (_:ys) = go (j - 1) ys
    go _ []     = Nothing
{-# INLINE (!!?) #-}

guessTitle :: MPD.Song -> String
guessTitle song = maybe (FP.takeBaseName $ MPD.toString $ MPD.sgFilePath song) (\x -> maybe "No title metadata" (MPD.toString) (listToMaybe x)) (C.lookup MPD.Title (MPD.sgTags song))

deriving instance G.Generic MPD.Status

instance A.ToJSON MPD.PlaybackState where
  toJSON v = A.String $ T.pack $ show v

instance A.ToJSON MPD.Volume where
  toJSON v = A.String $ T.pack $ show $ toInteger v

instance A.ToJSON MPD.Id where
  toJSON (MPD.Id v) = A.String $ T.pack $ show v

instance A.ToJSON MPD.Status
