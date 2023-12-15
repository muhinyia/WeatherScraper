-- |
-- Module      :  Database
-- Description :  Provides functions for database initialization, data insertion, retrieval, and deletion.
--                Interacts with an SQLite database to store and manage weather information.
--
-- This module utilizes the 'SQLite.Simple' library for interacting with the SQLite database
--
-- The database has two tables: 'temperature' and 'location' which have a 1:1 relationship
--
-- The exposed functions include:
--
-- - 'initializeDatabase': Initializes the database by creating 'temperature' and 'location' tables.
--
-- - 'insertWeatherData': Inserts weather information into the 'temperature' and 'location' tables.
--
-- - 'getAllWeatherData': Retrieves all weather data from the database, including date, temperature, city, latitude, and longitude.
--
-- - 'filterDataByCity': Filters weather data based on the specified city.
--
-- - 'viewDatabase': Prints the status of the database, including the number of tables, their names, and the total number of rows.
--
-- - 'deleteDatabase': Deletes all data from the database, including tables and their contents.
-- This module exposes the data fetched by above methods

{-# LANGUAGE OverloadedStrings #-}
module Database (initializeDatabase, insertWeatherData, getAllWeatherData, filterDataByCity, viewDatabase, deleteDatabase) where
import Parse
import Control.Monad (forM_)
import Data.Text (Text)
import Database.SQLite.Simple
import Types
import Data.Text (unpack)
import Data.String (fromString)

-- | Initializes the database by creating SQLITE3 database weather.db and tables: 'temperature' and 'location' tables.
--
initializeDatabase :: IO ()
initializeDatabase = do
  -- Create a new database
  conn <- open "weather.db"
  execute_ conn "DROP TABLE IF EXISTS temperature"
  execute_ conn "DROP TABLE IF EXISTS location"
  execute_ conn "CREATE TABLE temperature (id INTEGER PRIMARY KEY, date TEXT, temperature REAL)"
  execute_ conn "CREATE TABLE location (id INTEGER PRIMARY KEY, city TEXT, latitude REAL, longitude REAL)"
  close conn


-- | View the database, print out the number of tables created and their names
viewDatabase :: IO ()
viewDatabase = do
  putStrLn "\n######Database Status:######\n"
  conn <- open "weather.db"

  -- Fetch table names
  tables <- query_ conn "SELECT name FROM sqlite_master WHERE type='table';" :: IO [Only Text]

  -- Print table names
  putStrLn $ "Number of tables: " ++ show (length tables)
  putStrLn "\nTable names:\n"
  forM_ tables $ \table -> do
    let tableName = unpack (fromOnly table)
    putStrLn $ "Table: " ++ tableName

    -- Fetch and print row count for each table
    rowCount <- query_ conn (fromString $ "SELECT COUNT(*) FROM " <> tableName) :: IO [Only Int]
    let totalRows = fromOnly (head rowCount)
    putStrLn $ "Total rows in " ++ tableName ++ ": " ++ show totalRows

  close conn

-- | Deletes all data from the database
--
deleteDatabase :: IO ()
deleteDatabase = do
  putStrLn "\nDeleting all Data..."
  conn <- open "weather.db"
  execute_ conn "DELETE FROM temperature"
  execute_ conn "DELETE FROM location"
  close conn

-- | Inserts weather information into the 'temperature' and 'location' tables.
--
insertWeatherData :: WeatherInfo -> IO ()
insertWeatherData weatherInfo = do
  conn <- open "weather.db"
  execute conn "INSERT INTO temperature (date, temperature) VALUES (?, ?)" (date weatherInfo, temperature weatherInfo)
  execute conn "INSERT INTO location (city, latitude, longitude) VALUES (?, ?, ?)" (cityName (location weatherInfo), latitude (location weatherInfo), longitude (location weatherInfo))
  close conn

-- | Retrieves all weather data from the database, including date, temperature, city, latitude, and longitude.
--
getAllWeatherData :: IO [(Text, Double, Text, Double, Double)]
getAllWeatherData = do
  conn <- open "weather.db"
  rows <- query_ conn "SELECT t.date, t.temperature, l.city, l.latitude, l.longitude FROM temperature t JOIN location l ON t.id = l.id"
  close conn
  return rows


-- | Filters weather data based on the specified city and retrieves relevant information.
--
filterDataByCity:: Text -> IO [(Text, Double, Text, Double, Double)]
filterDataByCity userCity = do
    conn <- open "weather.db"
    rows <- query conn "SELECT t.date, t.temperature, l.city, l.latitude, l.longitude FROM temperature t JOIN location l ON t.id = l.id WHERE l.city = ?" (Only userCity)
    close conn
    return rows