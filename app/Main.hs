module Main (main) where

import Fetch (fetchWeatherData)
import Parse (extractWeatherInfo)
import Database
import Data.Foldable (forM_)
import Control.Monad (when)
import Data.Text (pack)
main :: IO ()
main = do
    putStrLn "\n=====================    WELCOME TO THE WEATHER DATA APP   ==============\nPlease Choose an Option to continue:\n"
    putStrLn "1. Initialize Database"
    putStrLn "2. View Database Status"
    putStrLn "3. Fetch Data From API"
    putStrLn "4. Retrieve All Data"
    putStrLn "5. Query Data By City"
    putStrLn "6. Delete All Data"
    putStrLn "7. Exit"
    putStrLn "Enter your choice: "
    choice <- getLine
    when (choice /= "7") $ do
        handleOption choice
        main

handleOption :: String -> IO ()
handleOption "1" = initializeAndFetchData
handleOption "2" = viewDatabaseStatus
handleOption "3" = fetchAndInsertData
handleOption "4" = viewAllData
handleOption "5" = viewDataPerCity
handleOption "6" = removeDatabase
handleOption _   = putStrLn "Invalid choice."


initializeAndFetchData :: IO ()
initializeAndFetchData = do
    initializeDatabase
    putStrLn "Database initialized. Now continue to Fetch and insert data..."
    

viewDatabaseStatus :: IO ()
viewDatabaseStatus = do
    viewDatabase


removeDatabase :: IO ()
removeDatabase = do
    putStrLn "Are you sure you want to delete the database? (yes/no): "
    confirmation <- getLine
    case confirmation of
        "yes" -> deleteDatabase
        "no"  -> putStrLn "Deletion canceled."
        _     -> putStrLn "Invalid input. Deletion canceled."

    

fetchAndInsertData :: IO ()
fetchAndInsertData = do
    --putStrLn "Enter your OpenWeatherMap API key: "
    --apiKey <- getLine
    putStrLn "Enter latitude: "
    latitude <- getLine
    putStrLn "Enter longitude: "
    longitude <- getLine

    weatherData <- fetchWeatherData latitude longitude
    case weatherData of
        Left err -> putStrLn $ "Error fetching weather data: " ++ err
        Right jsonData -> do
            case extractWeatherInfo jsonData of
                Left err -> putStrLn $ "Error extracting daily temperatures: " ++ err
                Right dailyTemperatures -> do
                    putStrLn "\n Data fetched from API.\nInserting data into the database...\n"
                    mapM_ insertWeatherData dailyTemperatures
                    putStrLn "Data inserted successfully."
                    
viewAllData :: IO ()
viewAllData = do
    allData <- getAllWeatherData
    putStrLn "\n===============================================All Data=======================================\n"
    forM_ allData $ \(date, temp, city, lat, lon) -> do
        putStrLn $ "Date: " ++ show date ++ ", Temperature: " ++ show temp ++ ", City: " ++ show city ++ ", Latitude: " ++ show lat ++ ", Longitude: " ++ show lon


-- View data for a specific city
viewDataPerCity :: IO ()
viewDataPerCity = do
    putStrLn "Enter the city to view data for: "
    userCity <- getLine
    dataForCity <- filterDataByCity (pack userCity)
    putStrLn $ "\n===============================================Data for " ++ userCity ++ "=======================================\n"
    forM_ dataForCity $ \(date, temp, city, lat, lon) ->
        putStrLn $ "Date: " ++ show date ++ ", Temperature: " ++ show temp ++ ", City: " ++ show city ++ ", Latitude: " ++ show lat ++ ", Longitude: " ++ show lon
