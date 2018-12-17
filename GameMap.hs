{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module GameMap( GameMap(..)
              , Tileset(..)
              , TilesetLink(..)
              , Layer(..)
              ) where

import GHC.Generics
import Data.Aeson

data GameMap = GameMap{
    width             :: Int,
    height            :: Int,
    tilewidth         :: Int,
    tileheight        :: Int,
    nextobjectid      :: Int,
    tileset           :: Tileset,
    layers            :: [Layer]
} deriving (Show,Generic)


data Tileset = Tileset{
    firstgid          :: Int,
    source            :: String
} deriving (Show,Generic)

data TilesetLink = TilesetLink{
    columns          :: Int,
    image            :: String
} deriving (Show,Generic)

data Layer = Layer{
    id                :: Int,
    name              :: String,
    width             :: Int,
    height            :: Int,
    layer_data        :: [Int]
} deriving (Show,Generic)

instance FromJSON GameMap
instance FromJSON Tileset
instance FromJSON TilesetLink
instance FromJSON Layer
