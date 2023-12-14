module Fetch (fetchWeatherData, constructApiUrl) where

import Network.HTTP.Simple
import qualified Data.Aeson as Aeson

constructApiUrl :: String -> String -> String
constructApiUrl latitude longitude =
  "https://api.openweathermap.org/data/2.5/forecast?lat=" ++ latitude ++ "&lon=" ++ longitude ++ "&appid=01c141fb7b86fc6539474bd27e5a01dd"

fetchWeatherData :: String -> String -> IO (Either String Aeson.Value)
fetchWeatherData latitude longitude = do
    let apiUrl = constructApiUrl latitude longitude
    request <- parseRequest apiUrl
    response <- httpLBS request
    let weatherDataBytes = getResponseBody response
    return $ Aeson.eitherDecode weatherDataBytes
