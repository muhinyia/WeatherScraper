module Fetch (fetchWeatherData) where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as Aeson

fetchWeatherData :: IO (Either String Aeson.Value)
fetchWeatherData = do
    let apiKey = "01c141fb7b86fc6539474bd27e5a01dd"
        apiUrl = "https://api.openweathermap.org/data/2.5/forecast?lat=44.34&lon=10.99&appid=" ++ apiKey
    request <- parseRequest apiUrl
    response <- httpLBS request
    let weatherDataBytes = getResponseBody response
    return $ Aeson.eitherDecode weatherDataBytes
