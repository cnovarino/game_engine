{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE DefaultSignatures #-}

module GameMap( GameMap(..)
              , Tileset(..)
              , Layer(..)
              , loadMap
              ) where

import GHC.Generics
import Data.Aeson
import Data.Monoid
import Control.Applicative
import qualified Data.ByteString.Lazy as BS

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

data Layer = Layer{
    id                :: Int,
    name              :: String,
    width             :: Int,
    height            :: Int,
    layer_data        :: [Int]
} deriving (Show,Generic)

instance FromJSON GameMap
instance FromJSON Tileset
instance FromJSON Layer

loadMap :: FromJSON a => FilePath -> IO (Maybe a)
loadMap filePath = do
                file <- BS.readFile filePath
                return $ decode file
