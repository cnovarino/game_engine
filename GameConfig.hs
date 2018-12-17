{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module GameConfig( GameConfig(..)
                 , PlayerConfig(..)
                 ) where

import GHC.Generics
import Data.Aeson

data GameConfig = GameConfig{
    game_title        :: String,
    game_width        :: Int,
    game_height       :: Int,
    player_config     :: PlayerConfig
} deriving (Show,Generic)


data PlayerConfig = PlayerConfig{
    player_sprite    :: String,
    init_x           :: Int,
    init_y           :: Int
} deriving (Show,Generic)


instance FromJSON GameConfig
instance FromJSON PlayerConfig
