module Utility where
import qualified Network.MPD as MPD

data SongList = SongList { songId :: Int, songName :: String } deriving Generic
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
