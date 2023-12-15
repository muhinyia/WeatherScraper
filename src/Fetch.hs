-- |
-- Module      :  Fetch
-- Description :  Provides functions for fetching weather data from the OpenWeatherMap API.
--
-- These functions are designed to be used together for retrieving weather data from the OpenWeatherMap API.
--
-- The 'constructApiUrl' function takes latitude and longitude as arguments and returns a formatted API URL.
--
-- The 'fetchWeatherData' function takes latitude and longitude as arguments, constructs the API URL using 'constructApiUrl',
-- sends a request to the OpenWeatherMap API, and returns the weather data as a JSON value wrapped in an 'Either' monad.
-- This module exposes two functions: 'constructApiUrl' and 'fetchWeatherData'.

module Fetch (fetchWeatherData, constructApiUrl) where

import Network.HTTP.Simple
import qualified Data.Aeson as Aeson

-- | Constructs the OpenWeatherMap API URL based on latitude and longitude.
--
-- Example:
--
-- > constructApiUrl "40.7128" "-74.0060"
-- > -- Returns: "https://api.openweathermap.org/data/2.5/forecast?lat=40.7128&lon=-74.0060&appid=01c141fb7b86fc6539474bd27e5a01dd"
constructApiUrl :: String -> String -> String    -- ^ Latitude -- ^ Longitude -- ^ Constructed API URL
constructApiUrl latitude longitude =
  "https://api.openweathermap.org/data/2.5/forecast?lat=" ++ latitude ++ "&lon=" ++ longitude ++ "&appid=01c141fb7b86fc6539474bd27e5a01dd"



 
-- | Fetches weather data from the OpenWeatherMap API based on latitude and longitude.
--
-- Example:
--
-- > fetchWeatherData "40.7128" "-74.0060"
-- > -- Returns: IO (Either String Aeson.Value) 
fetchWeatherData :: String -> String -> IO (Either String Aeson.Value)
fetchWeatherData latitude longitude = do
    let apiUrl = constructApiUrl latitude longitude
    request <- parseRequest apiUrl
    response <- httpLBS request
    let weatherDataBytes = getResponseBody response
    return $ Aeson.eitherDecode weatherDataBytes