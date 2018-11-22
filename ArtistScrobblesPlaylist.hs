{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- 835789484db9bf9715aa1fd908be19f9 - API key
import Network.HTTP.Client
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T

import Data.Functor.Identity

apiKey :: String
apiKey = "835789484db9bf9715aa1fd908be19f9"

data SongPlays =  SongPlays {name :: String, playcount :: String} deriving Show
newtype SongList = SongList {list :: [SongPlays] } deriving Show

instance FromJSON SongList where
    parseJSON = withObject "SongList" $ \o -> do
            topSongsO <- o .: "toptracks"
            songList     <- topSongsO .: "track"
            return $ SongList songList

instance FromJSON SongPlays where
        parseJSON = withObject "SongPlays" $ \o -> do
          nname <- o .: "name"
          nplaycount  <- o .: "playcount"
          return $ SongPlays nname nplaycount

printPlaylist :: Maybe SongList -> IO ()
printPlaylist Nothing = putStrLn "api request/decoding failed"
printPlaylist (Just (SongList [])) = putStr ""
printPlaylist (Just (SongList (sph:spt))) = do
        putStrLn (sname ++ " - " ++ "Plays:" ++ show splays) 
        printPlaylist (Just (SongList spt))
                where SongPlays sname splays = sph

main :: IO ()
main = do
    let settings = managerSetProxy
            (proxyEnvironment Nothing)
            defaultManagerSettings
    man <- newManager settings
    putStrLn "Input desired artist"
    artist <- getLine
    putStrLn "Input playlist length"
    limit <- getLine
    reqURL <- parseRequest ("http://ws.audioscrobbler.com/2.0/?method=artist.gettoptracks" ++ 
                                "&artist=" ++ artist ++
                                "&api_key=" ++ apiKey ++ 
                                "&limit=" ++ limit ++
                                "&format=json")
    print ("http://ws.audioscrobbler.com/2.0/?method=artist.gettoptracks" ++ 
        "&artist=" ++ artist ++
        "&api_key=" ++ apiKey ++ 
        "&limit=" ++ limit ++
        "&format=json")
    let req = reqURL
            -- Note that the following settings will be completely ignored.
              { proxy = Just $ Proxy "localhost" 1234
              }
    response <- httpLbs req man
    printPlaylist (decode (responseBody response) :: Maybe SongList)