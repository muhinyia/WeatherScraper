module Types(WeatherData(..), TemperatureData(..), Location(..)) where


data WeatherData = WeatherData
    { dt_txt :: String
    , main :: TemperatureData
    } deriving (Show)

data TemperatureData = TemperatureData
    { temp :: Float
    } deriving (Show)

data Location = Location
    { cityName :: String
    , latitude :: Float
    , longitude :: Float
    } deriving (Show)
