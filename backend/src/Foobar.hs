{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Foobar 
  ( sumUp
  , Piece (..)
  , Person (..)
  ) where

-- module DataType (PlaceHolder (..)) where
-- data Placeholder a = Foo a | Bar | Baz deriving (Show)

import           Data.Aeson
import           GHC.Generics
import           Data.Text           (Text)

-- small file used for trying to figure out local imports for server-/clientDemo

sumUp :: Int -> Int -> Int
sumUp a b = a + b

data Piece = Bishop | Knight deriving (Generic, Show)
instance ToJSON Piece where
    -- No need to provide a toJSON implementation.
    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Piece
    -- No need to provide a parseJSON implementation.


data Person = Person {
      name :: Text
    , age  :: Int
    } deriving (Generic, Show)

instance ToJSON Person where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

-- instance FromJSON Person
--     -- No need to provide a parseJSON implementation.
instance FromJSON Person where
  parseJSON = withObject "Person" $ \v -> Person
      <$> v .: "name"
      <*> v .: "age"





{-
import data.text
import qualified data.text as Text
-}