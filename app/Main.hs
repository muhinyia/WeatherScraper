module Main (main) where

import Fetch (fetchWeatherData)
import Parse (extractWeatherInfo)

main :: IO ()
main = do
    weatherData <- fetchWeatherData
    case weatherData of
        Left err -> putStrLn $ "Error fetching weather data: " ++ err
        Right jsonData -> do
            case extractWeatherInfo jsonData of
                Left err -> putStrLn $ "Error extracting daily temperatures: " ++ err
                Right dailyTemperatures -> do
                    putStrLn "Helllooo Daily Temperature Information:"
                    mapM_ print dailyTemperatures
