{-# LANGUAGE OverloadedStrings #-}

module Parse (extractWeatherInfo) where
import Types
import Data.Aeson
import Data.Aeson.Types (Parser, withObject, (.!=), (.:), (.:?), parseMaybe, parseEither)


instance FromJSON WeatherData where
    parseJSON = withObject "WeatherData" $ \v -> do
        dt <- v .: "dt_txt"
        main' <- v .: "main"
        return WeatherData { dt_txt = dt, main = main' }

instance FromJSON TemperatureData where
    parseJSON = withObject "TemperatureData" $ \v -> do
        temp' <- v .: "temp"
        return TemperatureData { temp = temp' }

instance FromJSON Location where
    parseJSON = withObject "Location" $ \v -> do
        name <- v .: "name"
        coord <- v .: "coord"
        lat <- coord .: "lat"
        lon <- coord .: "lon"
        return Location { cityName = name, latitude = lat, longitude = lon }

data WeatherInfo = WeatherInfo
    { date :: String
    , temperature :: Float
    , location :: Location
    } deriving (Show)

extractWeatherInfo :: Value -> Either String [WeatherInfo]
extractWeatherInfo jsonData = case parseEither parseWeatherInfo jsonData of
    Left err -> Left err
    Right weatherInfo -> Right weatherInfo

parseWeatherInfo :: Value -> Parser [WeatherInfo]
parseWeatherInfo = withObject "WeatherInfo" $ \v -> do
    locationData <- v .: "city"
    weatherDataList <- v .: "list"
    mapM (parseWeatherData locationData) weatherDataList

parseWeatherData :: Location -> Value -> Parser WeatherInfo
parseWeatherData locationData = withObject "WeatherData" $ \v -> do
    date <- v .: "dt_txt"
    main' <- v .: "main"
    let temperatureData = TemperatureData { temp = temp main' }
    return WeatherInfo { date = date, temperature = temp temperatureData, location = locationData }
