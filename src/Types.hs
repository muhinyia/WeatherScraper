module Types where

import Data.Aeson

data Weather = Weather {
    -- Define your fields here
} deriving Show

instance FromJSON Weather where
    parseJSON = undefined -- Implement your JSON parsing logic here
