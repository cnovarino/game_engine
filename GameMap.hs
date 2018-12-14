{-# LANGUAGE DuplicateRecordFields #-}

module GameMap where

import qualified  Data.ByteString as B

data GameMap = GameMap{
    version           :: String,
    tiledversion      :: String,
    orientation       :: String,
    renderorder       :: String,
    width             :: Int,
    height            :: Int,
    tilewidth         :: Int,
    tileheight        :: Int,
    infinite          :: Bool,
    nextlayerid       :: Int,
    nextobjectid      :: Int,
    tileset           :: Tileset,
    layers            :: [Layer]
} deriving (Show,Eq)


data Tileset = Tileset{
    firstgid          :: Int,
    source            :: String
} deriving (Show,Eq)

data Layer = Layer{
    id                :: Int,
    name              :: String,
    width             :: Int,
    height            :: Int,
    layer_data        :: String
} deriving (Show,Eq)
