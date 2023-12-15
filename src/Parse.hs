-- |
-- Module      :  Parse
-- Description :  Provides functions for parsing JSON data into Haskell data types.
--                Defines instances for 'FromJSON' for the 'WeatherData', 'TemperatureData', and 'Location' types.
--
-- This module relies on the 'Data.Aeson' library for JSON parsing and the 'Types' module for data type definitions.
--
-- The 'extractWeatherInfo' function takes a 'Value' (typically a JSON object) and attempts to parse it into a list
-- of 'WeatherInfo' values, returning either an error message or the successfully parsed weather information.
-- This module exposes the Parsed Data.


{-# LANGUAGE OverloadedStrings #-}

module Parse (extractWeatherInfo, WeatherInfo(..)) where
import Types
import Data.Aeson
import Data.Aeson.Types (Parser, withObject, (.!=), (.:), (.:?), parseMaybe, parseEither)

-- | Parses JSON objects into 'WeatherData' type.
instance FromJSON WeatherData where
    parseJSON = withObject "WeatherData" $ \v -> do
        dt <- v .: "dt_txt"
        main' <- v .: "main"
        return WeatherData { dt_txt = dt, main = main' }

-- | Parses JSON objects into 'TemperatureData' type.
instance FromJSON TemperatureData where
    parseJSON = withObject "TemperatureData" $ \v -> do
        temp' <- v .: "temp"
        return TemperatureData { temp = temp' }

-- | Parses JSON objects into 'Location' type.
instance FromJSON Location where
    parseJSON = withObject "Location" $ \v -> do
        name <- v .: "name"
        coord <- v .: "coord"
        lat <- coord .: "lat"
        lon <- coord .: "lon"
        return Location { cityName = name, latitude = lat, longitude = lon }

-- | Represents structured weather information, including date, temperature, and location details.
data WeatherInfo = WeatherInfo
    { date :: String           -- ^ Date and time of the weather data.
    , temperature :: Float     -- ^ Temperature value.
    , location :: Location     -- ^ Geographical location details.
    } deriving (Show)


-- | Extracts weather information from a JSON 'Value' and parses it into a list of 'WeatherInfo'.
extractWeatherInfo :: Value -> Either String [WeatherInfo]  -- ^ JSON value containing weather information.
extractWeatherInfo jsonData = case parseEither parseWeatherInfo jsonData of -- ^ Result wrapped in 'Either' monad.
    Left err -> Left err
    Right weatherInfo -> Right weatherInfo


-- | Parses a JSON 'Value' into a list of 'WeatherInfo' using the provided location data.
parseWeatherInfo :: Value -> Parser [WeatherInfo]
parseWeatherInfo = withObject "WeatherInfo" $ \v -> do
    locationData <- v .: "city"
    weatherDataList <- v .: "list"
    mapM (parseWeatherData locationData) weatherDataList


-- | Parses a JSON 'Value' into 'WeatherInfo' using the provided location data.
parseWeatherData :: Location -> Value -> Parser WeatherInfo
parseWeatherData locationData = withObject "WeatherData" $ \v -> do
    date <- v .: "dt_txt"
    main' <- v .: "main"
    let temperatureData = TemperatureData { temp = temp main' }
    return WeatherInfo { date = date, temperature = temp temperatureData, location = locationData }