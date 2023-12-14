{-# LANGUAGE OverloadedStrings #-}
module Database (initializeDatabase, insertWeatherData, getAllWeatherData, filterDataByCity, viewDatabase, deleteDatabase) where
import Parse
import Control.Monad (forM_)
import Data.Text (Text)
import Database.SQLite.Simple
import Types
import Data.Text (unpack)
import Data.String (fromString)


-- Initialize database with two tables
initializeDatabase :: IO ()
initializeDatabase = do
  -- Create a new database
  conn <- open "weather.db"
  execute_ conn "DROP TABLE IF EXISTS temperature"
  execute_ conn "DROP TABLE IF EXISTS location"
  execute_ conn "CREATE TABLE temperature (id INTEGER PRIMARY KEY, date TEXT, temperature REAL)"
  execute_ conn "CREATE TABLE location (id INTEGER PRIMARY KEY, city TEXT, latitude REAL, longitude REAL)"
  close conn


-- View the database, print out the number of tables created and their names
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

-- Delete the entire database, including all tables and data
deleteDatabase :: IO ()
deleteDatabase = do
  putStrLn "\nDeleting all Data..."
  conn <- open "weather.db"
  execute_ conn "DELETE FROM temperature"
  execute_ conn "DELETE FROM location"
  close conn

-- Insert weather data into the database
insertWeatherData :: WeatherInfo -> IO ()
insertWeatherData weatherInfo = do
  conn <- open "weather.db"
  execute conn "INSERT INTO temperature (date, temperature) VALUES (?, ?)" (date weatherInfo, temperature weatherInfo)
  execute conn "INSERT INTO location (city, latitude, longitude) VALUES (?, ?, ?)" (cityName (location weatherInfo), latitude (location weatherInfo), longitude (location weatherInfo))
  close conn

-- Get all weather data from the database
getAllWeatherData :: IO [(Text, Double, Text, Double, Double)]
getAllWeatherData = do
  conn <- open "weather.db"
  rows <- query_ conn "SELECT t.date, t.temperature, l.city, l.latitude, l.longitude FROM temperature t JOIN location l ON t.id = l.id"
  close conn
  return rows



  -- Filter weather data by city
filterDataByCity:: Text -> IO [(Text, Double, Text, Double, Double)]
filterDataByCity userCity = do
    conn <- open "weather.db"
    rows <- query conn "SELECT t.date, t.temperature, l.city, l.latitude, l.longitude FROM temperature t JOIN location l ON t.id = l.id WHERE l.city = ?" (Only userCity)
    close conn
    return rows