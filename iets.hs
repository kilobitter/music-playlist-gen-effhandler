{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- 835789484db9bf9715aa1fd908be19f9 - API key
import           Control.Monad.Trans.Maybe
import Network.HTTP.Client
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import Control.Monad.Trans

import Data.Functor.Identity

apiKey :: String
apiKey = "835789484db9bf9715aa1fd908be19f9"

data SongPlays =  SongPlays {name :: String, playcount :: Int} deriving Show
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
          return $ SongPlays nname (read nplaycount :: Int)

printPlaylist :: Artist -> SongList -> IO ()
-- printPlaylist artist Nothing = putStrLn ("api request/decoding failed for artist: " ++ artist)
printPlaylist _ (SongList []) = putStr ""
printPlaylist artist (SongList (sph:spt)) = do
        putStrLn (artist ++ " - " ++ sname ++ " - " ++ "Plays:" ++ show splays) 
        printPlaylist artist (SongList spt)
                where SongPlays sname splays = sph

playlistToString :: Artist -> SongList -> Identity String
-- playlistToString artist Nothing = "api request/decoding failed for artist: "
playlistToString _ (SongList []) = ""
playlistToString artist (SongList (sph:spt)) = Identity
        (artist ++ " - " ++ sname ++ " - " ++ "Plays:" ++ show splays ++ "\n" ++
        runIdentity (playlistToString artist (SongList spt)))
                where SongPlays sname splays = sph

type Artist = String
type Limit = String

getArtistMock :: Identity String
getArtistMock = "Bones"

getLimitMock :: Identity String
getLimitMock = "3"

requestMock :: Artist -> Limit -> Identity SongList
requestMock art lim
    | art == "Bones" && lim == "3" = Identity (SongList [SongPlays "Dirt" 25, SongPlays "hdmi"  18, SongPlays "Corduroy" 7])
    | otherwise = error "Called with wrong arguments"

lastFmApiRequest :: Artist -> Limit -> MaybeT IO SongList
lastFmApiRequest artist limit = do
  let settings = managerSetProxy
            (proxyEnvironment Nothing)
            defaultManagerSettings
  man <- lift (newManager settings)
  reqURL <- parseRequest ("http://ws.audioscrobbler.com/2.0/?method=artist.gettoptracks" ++ 
                           "&artist=" ++ artist ++
                           "&api_key=" ++ apiKey ++ 
                           "&limit=" ++ limit ++
                           "&format=json")
  let req = reqURL
            -- Note that the following settings will be completely ignored.
            { proxy = Just $ Proxy "localhost" 1234
            }
  response <- lift (httpLbs req man)
  MaybeT (return (decode (responseBody response) :: Maybe SongList))

getArtistIO :: IO Artist
getArtistIO = do
  putStrLn "Input desired artist"
  getLine

getLimitIO :: IO Artist
getLimitIO = do
  putStrLn "Input desired limit"
  getLine

  -- 
artistRequest :: (Monad f) => f Artist -> f Limit -> (Artist -> Limit -> f SongList) -> (Artist -> SongList -> f a) -> f a
artistRequest getArtist getLimit getPopSongs printPL = do
    artist <- getArtist
    limit <- getLimit
    result <- getPopSongs artist limit
    printPL artist result

main :: IO ()
main = putStr (runIdentity (artistRequest getArtistMock getLimitMock requestMock playlistToString))
--main = fmap (const ()) (runMaybeT (artistRequest (lift getArtistIO) (lift getLimitIO) lastFmApiRequest (\a b -> lift (printPlaylist a b))))

--     print ("http://ws.audioscrobbler.com/2.0/?method=artist.gettoptracks" ++ 
--         "&artist=" ++ artist ++
--         "&api_key=" ++ apiKey ++ 
--         "&limit=" ++ limit ++
--         "&format=json")