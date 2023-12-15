-- |
-- Module      :  Main
-- Description :  Main module for the Weather Data App, providing a command-line interface.
--                Allows users to interact with the app by initializing the database, fetching data from an API,
--                viewing database status, retrieving and querying weather data, and deleting all data.
--

-- The main entry point is the 'main' function, which displays a menu for users to choose options.
--
-- Example usage:
--
-- > main
--
-- The app provides the following options:
--
-- - Initialize Database: Initializes the database by creating tables.
--
-- - View Database Status: Displays the current status of the database, including the number of tables and their names.
--
-- - Fetch Data From API: Fetches weather data from the OpenWeatherMap API based on user input (latitude and longitude).
--
-- - Retrieve All Data: Retrieves all weather data from the database and displays it.
--
-- - Query Data By City: Allows the user to enter a city name and retrieves weather data specific to that city.
--
-- - Delete All Data: Deletes all data from the database, including tables and their contents.
--
-- - Exit: Exits the application.


module Main (main) where
-- imports
import Fetch (fetchWeatherData)
import Parse (extractWeatherInfo)
import Database
import Data.Foldable (forM_)
import Control.Monad (when)
import Data.Text (pack)

-- | The main entry point for the Weather Data App.
--
-- Displays a menu for users to choose options.
--
main :: IO ()
main = do
    putStrLn "\n=====================    WELCOME TO THE TEMPERATURE DATA APP   ==============\nPlease Choose an Option to continue:\n"
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

-- | Handles the user's choice based on the selected menu option.
--
handleOption :: String -> IO ()
handleOption "1" = initializeTheDatabase
handleOption "2" = viewDatabaseStatus
handleOption "3" = fetchAndInsertData
handleOption "4" = viewAllData
handleOption "5" = viewDataPerCity
handleOption "6" = removeDatabase
handleOption _   = putStrLn "Invalid choice."


-- | Initializes the database.
--
initializeTheDatabase :: IO ()
initializeTheDatabase = do
    initializeDatabase
    putStrLn "Database initialized. Now continue to Fetch and insert data..."
    
-- | Displays the current status of the database, including the number of tables and their names and total data rows
--
viewDatabaseStatus :: IO ()
viewDatabaseStatus = do
    viewDatabase

-- | Deletes all data from the database.
--
removeDatabase :: IO ()
removeDatabase = do
    putStrLn "Are you sure you want to delete the database? (yes/no): "
    confirmation <- getLine
    case confirmation of
        "yes" -> deleteDatabase
        "no"  -> putStrLn "Deletion canceled."
        _     -> putStrLn "Invalid input. Deletion canceled."

    
-- | Fetches weather data from the API based on user input (latitude and longitude)
--   and inserts the data into the database.
--
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

-- | Retrieves all weather data from the database and displays it.
--
viewAllData :: IO ()
viewAllData = do
    allData <- getAllWeatherData
    putStrLn "\n===============================================All Data=======================================\n"
    forM_ allData $ \(date, temp, city, lat, lon) -> do
        putStrLn $ "Date: " ++ show date ++ ", Temperature: " ++ show temp ++ ", City: " ++ show city ++ ", Latitude: " ++ show lat ++ ", Longitude: " ++ show lon


-- | Retrieves and displays weather data for a specific city based on user input.
--
viewDataPerCity :: IO ()
viewDataPerCity = do
    putStrLn "Enter the city to view data for: "
    userCity <- getLine
    dataForCity <- filterDataByCity (pack userCity)
    putStrLn $ "\n===============================================Data for " ++ userCity ++ "=======================================\n"
    forM_ dataForCity $ \(date, temp, city, lat, lon) ->
        putStrLn $ "Date: " ++ show date ++ ", Temperature: " ++ show temp ++ ", City: " ++ show city ++ ", Latitude: " ++ show lat ++ ", Longitude: " ++ show lon