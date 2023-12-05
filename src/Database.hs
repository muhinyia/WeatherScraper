module Database (createConnection, logWeatherData) where

import Database.SQLite.Simple
import qualified Data.ByteString.Lazy.Char8 as LBS

-- Create a connection to the SQLite database
createConnection :: IO Connection
createConnection = open "weather_data.db"

-- Function to log weather data
logWeatherData :: LBS.ByteString -> IO ()
logWeatherData weatherData = do
    putStrLn "Received weather data:"
    LBS.putStrLn weatherData
