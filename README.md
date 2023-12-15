# Weather Data App

## Overview

This Haskell group project, the Weather Data App, is designed to fetch, store, and manipulate weather data. The app is structured into several modules, each serving a specific purpose:

- **Types Module:** Defines Haskell types for storing temperature and location data.
- **Fetch Module (Fetch.hs):** Fetches weather data from the OpenWeatherMap API using latitude and longitude.
- **Parse Module (Parse.hs):** Parses the fetched data into Haskell types defined in the Types module.
- **Database Module (Database.hs):** Interacts with an SQLite database, allowing users to initialize, view, and delete data.

The main functionality is encapsulated in the **Main Module (Main.hs)**, which provides a command-line interface for users to interact with the app. Users can perform various actions such as initializing the database, fetching data from the API, viewing the database status, retrieving and querying weather data, and deleting all data.

## Main Features

### 1. Initialize Database

Users can initialize the database by creating necessary tables to store temperature and location data.

### 2. View Database Status

The app allows users to view the current status of the database, including the number of tables and their names.

### 3. Fetch Data From API

Users can fetch weather data from the OpenWeatherMap API by providing latitude and longitude inputs.

### 4. Retrieve All Data

Retrieve and display all weather data stored in the database, including date, temperature, city, latitude, and longitude.

### 5. Query Data By City

Users can query weather data specific to a city by entering the city name.

### 6. Delete All Data

Delete all data from the database, including tables and their contents.

### 7. Exit

Users can exit the application when done with the chosen actions.

## Getting Started

To run the Weather Data App:

1. Clone the repository to your local machine.
2. Navigate to the project root directory.
3. Run the following commands:

    ```bash
    stack build
    stack exec haskell-group-project-exe
    ```

4. Follow the on-screen instructions to interact with the app.

To clear stack, run:

```bash
stack clean
```

Feel free to contribute and provide feedback!