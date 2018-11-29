{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- 835789484db9bf9715aa1fd908be19f9 - API key
import Network.HTTP.Client
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T

import Data.Functor.Identity

apiKey :: String
apiKey = "58eb085d-e1b6-4d6f-a070-f35e860dd4fe"

-- API Key = 58eb085d-e1b6-4d6f-a070-f35e860dd4fe
--this is for setlist.fm

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
    reqURL <- parseRequest ("https://api.setlist.fm/rest/1.0/artist/" ++ 
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