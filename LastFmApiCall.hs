{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- 835789484db9bf9715aa1fd908be19f9 - API key
import Network.HTTP.Client
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T

import Data.Functor.Identity

type Program a =
        forall f. (Monad f) => (String -> f String) -> f a

program :: Program Int
program read = do
        content <- read "myfile.txt"
        return (length (T.lines content))

runProgram = program readFile
testProgram = runIdentity $ program (\_ -> return "contents")

apiKey :: String
apiKey = "835789484db9bf9715aa1fd908be19f9"

newtype ArtistList = ArtistList{list :: [ArtistPlays]} deriving Show
data ArtistPlays = ArtistPlays {name :: String, playcount :: String} deriving Show

instance FromJSON ArtistList where
        parseJSON = withObject "ArtistList" $ \o -> do
                topArtistsO <- o .: "topartists"
                artistList     <- topArtistsO .: "artist"
                return $ ArtistList artistList

instance FromJSON ArtistPlays where
        parseJSON = withObject "ArtistPlays" $ \o -> do
          name <- o .: "name"
          playcount  <- o .: "playcount"
          return $ ArtistPlays name playcount

main :: IO ()
main = do
    let settings = managerSetProxy
            (proxyEnvironment Nothing)
            defaultManagerSettings
    man <- newManager settings
    putStrLn "Input desired user"
    user <- getLine
    putStrLn "Artist limit?"
    limit <- getLine
    reqURL <- parseRequest ("http://ws.audioscrobbler.com/2.0/?method=user.gettopartists" ++ 
                                "&user=" ++ user ++
                                "&api_key=" ++ apiKey ++ 
                                "&limit=" ++ limit ++
                                "&format=json")
    let req = reqURL
            -- Note that the following settings will be completely ignored.
              { proxy = Just $ Proxy "localhost" 1234
              }
    response <- httpLbs req man
    print (decode (responseBody response) :: Maybe ArtistList)