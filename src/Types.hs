-- |
-- Module      :  Types
-- Description :  Defines data types for temperature data, and location.
--
-- This module defines three data types: 'WeatherData', 'TemperatureData', and 'Location'.
-- The 'WeatherData' type contains a timestamp and the main temperature data for that timestamp.
--
-- The 'TemperatureData' type represents the temperature information, specifically the temperature value.
--
-- The 'Location' type encapsulates details about a geographical location, including the city name,
-- latitude, and longitude.
--
-- This module exposes three Types: 'WeatherData', 'TemperatureData', and 'Location'.


module Types(WeatherData(..), TemperatureData(..), Location(..)) where


-- | Represents weather data, including a timestamp and main temperature data.
data WeatherData = WeatherData
    { dt_txt :: String         -- ^ Timestamp in text format (e.g., "2023-12-14 12:00:00").
    , main :: TemperatureData  -- ^ Main temperature data associated with the timestamp.
    } deriving (Show)


-- | Represents temperature data, including the temperature value.
data TemperatureData = TemperatureData
    { temp :: Float            -- ^ Temperature value in degrees Celsius.
    } deriving (Show)


-- | Represents location data, including the city name, latitude, and longitude.
data Location = Location
    { cityName :: String        -- ^ Name of the city.
    , latitude :: Float         -- ^ Latitude of the geographical location.
    , longitude :: Float        -- ^ Longitude of the geographical location.
    } deriving (Show)