{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import GameMap
import GameConfig
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (void)
import Control.Concurrent.Timer (oneShotTimer)
import Control.Concurrent.Suspend.Lifted (msDelay)
import Data.Maybe(fromJust)
import qualified Data.ByteString.Lazy as BS
import Data.Aeson

import Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Gui.QPixmap as QPixmap
import qualified Graphics.UI.Qtah.Gui.QKeyEvent as QKeyEvent
import qualified Graphics.UI.Qtah.Gui.QBrush as QBrush
import qualified Graphics.UI.Qtah.Gui.QColor as QColor
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QAbstractScrollArea as QAbstractScrollArea
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import qualified Graphics.UI.Qtah.Widgets.QGraphicsView as QGraphicsView
import qualified Graphics.UI.Qtah.Widgets.QGraphicsScene as QGraphicsScene
import qualified Graphics.UI.Qtah.Widgets.QGraphicsItem as QGraphicsItem
import qualified Graphics.UI.Qtah.Widgets.QGraphicsPixmapItem as QGraphicsPixmapItem
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Core.Types as Types
--import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Event (onEvent)
import System.Environment (getArgs)

data Direction = KeyUp | KeyDown | KeyLeft | KeyRight

directionVal :: Direction -> Int
directionVal KeyDown = 0
directionVal KeyLeft = 1
directionVal KeyRight = 2
directionVal KeyUp = 3

data GameUI = GameUI {
    scene             :: QGraphicsScene.QGraphicsScene,
    player            :: Player
}

data Player = Player {
    sprite            :: Sprite,
    name              :: String,
    level             :: Int,
    xp                :: Int,
    step              :: Int,
    direction         :: Direction
}

data Sprite = Sprite {
    spriteFile        :: QPixmap.QPixmap,
    pixmapItem        :: QGraphicsPixmapItem.QGraphicsPixmapItem,
    spriteStepWidth   :: Int,
    spriteStepHeight  :: Int,
    directions        :: Int,
    steps             :: Int
}

main :: IO ()
main = withScopedPtr (getArgs >>= QApplication.new) $ \_ -> do
    mbGameConfig <- (loadJson "./res/conf/conf.json" :: IO (Maybe GameConfig))

    case mbGameConfig of
        Nothing -> error "Error Loading Config File"
        Just (GameConfig _ _ _ _) -> do
            let gameConfig = fromJust mbGameConfig
            mbGameMap <- (loadJson "./res/maps/map2.json" :: IO (Maybe GameMap))

            case mbGameMap of
                Nothing -> error "Error Loading Map"
                Just (GameMap _ _ _ _ _ _ _) -> do
                    let gameMap = fromJust mbGameMap
                    mbTilesetLink <- (loadJson (source (tileset gameMap)) :: IO (Maybe TilesetLink))

                    case mbTilesetLink of
                        Nothing -> error "Error Loading TilesetLink"
                        Just (TilesetLink _ _) -> do
                            let tilesetLink = fromJust mbTilesetLink
                            player <- initPlayer (player_config gameConfig)
                            charRef <- newIORef player
                            game <- initGame gameConfig charRef gameMap tilesetLink
                            QWidget.show game
                            QCoreApplication.exec


initPlayer :: PlayerConfig -> IO Player
initPlayer playerConfig = do
    spriteFile <- QPixmap.newWithFile (player_sprite playerConfig)
    (spriteWidth :: Int ) <- QPixmap.width spriteFile
    (spriteHeight :: Int ) <- QPixmap.height spriteFile
    pixmapItem <- QGraphicsPixmapItem.new

    --QGraphicsItem.setPosRaw pixmapItem (init_x playerConfig) (init_y playerConfig)

    let
      currentDirection = KeyDown
      currentStep = 0
      directions = 4
      steps = 3
      spriteStepWidth = quot spriteWidth steps
      spriteStepHeight = quot spriteHeight directions
      sprite' = Sprite {
            spriteFile = spriteFile,
            pixmapItem = pixmapItem,
            spriteStepWidth = spriteStepWidth,
            spriteStepHeight = spriteStepHeight,
            directions = directions,
            steps = steps
      }

    void(updatePixmapFromSprite (currentStep * spriteStepWidth) ((directionVal currentDirection) * spriteStepHeight) spriteStepWidth spriteStepHeight  spriteFile pixmapItem)

    return Player {
          sprite = sprite',
          direction = currentDirection,
          step = currentStep,
          name = "Test",
          level = 1,
          xp = 0
    }

updatePixmapFromSprite :: Int -> Int -> Int -> Int -> QPixmap.QPixmap -> QGraphicsPixmapItem.QGraphicsPixmapItem -> IO ()
updatePixmapFromSprite x y width height sprite pixmapItem = do
    spriteFrame <- QPixmap.copyRaw sprite x y width height
    QGraphicsPixmapItem.setPixmap pixmapItem spriteFrame
    return ()

initGame :: GameConfig -> IORef Player -> GameMap -> TilesetLink -> IO QWidget.QWidget
initGame gameConfig playerRef gameMap tilesetLink = do
    widget <- QWidget.new
    gameScene <- QGraphicsScene.new
    graphicsView <- QGraphicsView.newWithSceneAndParent gameScene widget
    mainLayout <- QVBoxLayout.new
    player <- readIORef playerRef

    movingRef <- newIORef False

    QGraphicsView.setAlignment graphicsView (Types.toQtAlignment ( (Types.fromQtAlignment Types.alignLeft) + (Types.fromQtAlignment Types.alignTop)))

    backgroundColor <- QColor.newRgb 0 0 0
    backgroundBrush <-  QBrush.newWithColor backgroundColor

    QGraphicsScene.setBackgroundBrush gameScene backgroundBrush

    QAbstractScrollArea.setHorizontalScrollBarPolicy graphicsView Types.ScrollBarAlwaysOff
    QAbstractScrollArea.setVerticalScrollBarPolicy graphicsView Types.ScrollBarAlwaysOff
    QBoxLayout.addWidget mainLayout graphicsView

    QWidget.setWindowTitle widget (game_title gameConfig)
    QWidget.resizeRaw widget (game_width gameConfig) (game_height gameConfig)
    QWidget.setLayout widget mainLayout

    drawMap (pixmapItem (sprite player)) gameMap gameScene tilesetLink
    QGraphicsScene.addItem gameScene $ pixmapItem (sprite player)

    _ <- onEvent widget $ \(event :: QKeyEvent.QKeyEvent) -> do
      isMoving <- readIORef movingRef
      if isMoving
          then return False
          else startMovement movingRef playerRef event

    return widget


drawMap :: QGraphicsPixmapItem.QGraphicsPixmapItem -> GameMap -> QGraphicsScene.QGraphicsScene -> TilesetLink -> IO ()
drawMap player gameMap gameScene tilesetLink = do
    let tileset' = tileset gameMap
        width' = width (gameMap :: GameMap)
        height' = height (gameMap :: GameMap)
        tilewidth' = tilewidth (gameMap :: GameMap)
        tileheight' = tileheight (gameMap :: GameMap)
        columns' = columns tilesetLink
    tilesetFile <- QPixmap.newWithFile (image tilesetLink)
    drawLayers gameScene tilesetFile width' height' tilewidth' tileheight' columns' (layers gameMap)
    return ()

drawLayers :: QGraphicsScene.QGraphicsScene -> QPixmap.QPixmap -> Int -> Int -> Int -> Int -> Int -> [Layer] -> IO ()
drawLayers gameScene tilesetFile width height tilewidth tileheight columns [x] = drawLayer gameScene tilesetFile width height tilewidth tileheight columns 0 (layer_data x)
drawLayers gameScene tilesetFile width height tilewidth tileheight columns (x:xs) = do
    drawLayer gameScene tilesetFile width height tilewidth tileheight columns 0 (layer_data x)
    drawLayers gameScene tilesetFile width height tilewidth tileheight columns xs
drawLayers _ _ _ _ _ _ _ _ = return ()

drawLayer :: QGraphicsScene.QGraphicsScene -> QPixmap.QPixmap -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> IO ()
drawLayer _ _ _ _ _ _ _ _ [] = return ()

drawLayer gameScene tilesetFile width height tilewidth tileheight columns currentTile (0:xs) = drawLayer gameScene tilesetFile width height tilewidth tileheight columns (currentTile+1) xs

drawLayer gameScene tilesetFile width height tilewidth tileheight columns currentTile [x]  = do
    tile <- getTile tilesetFile tilewidth tileheight columns x
    pixmapItem <- QGraphicsPixmapItem.new
    QGraphicsPixmapItem.setPixmap pixmapItem tile
    QGraphicsScene.addItem gameScene pixmapItem
    QGraphicsItem.moveBy pixmapItem (fromIntegral ((getTileX width currentTile ) * tilewidth)) (fromIntegral ((getTileY width currentTile) * tileheight))

drawLayer gameScene tilesetFile width height tilewidth tileheight columns currentTile (x:xs) = do
    tile <- getTile tilesetFile tilewidth tileheight columns x
    pixmapItem <- QGraphicsPixmapItem.new
    QGraphicsPixmapItem.setPixmap pixmapItem tile
    QGraphicsScene.addItem gameScene pixmapItem
    QGraphicsItem.moveBy pixmapItem (fromIntegral ((getTileX width currentTile) * tilewidth)) (fromIntegral ((getTileY width currentTile) * tileheight))
    drawLayer gameScene tilesetFile width height tilewidth tileheight columns (currentTile+1) xs

getTile :: QPixmap.QPixmap -> Int -> Int -> Int -> Int -> IO QPixmap.QPixmap
getTile tilesetFile tilewidth tileheight columns tilePos = do
    extractedTile <- QPixmap.copyRaw tilesetFile ((getTileX columns tilePos) * tilewidth) ((getTileY columns tilePos) * tileheight) tilewidth tileheight
    return extractedTile

getTileX :: Int -> Int -> Int
getTileX cols pos = (mod pos cols) -1

getTileY :: Int -> Int -> Int
getTileY cols pos = quot pos cols

startMovement :: IORef Bool -> IORef Player -> QKeyEvent.QKeyEvent -> IO Bool
startMovement movingRef playerRef event = do
    writeIORef movingRef True
    _ <- oneShotTimer (writeIORef movingRef False) (msDelay 50)
    player <- readIORef playerRef
    player' <- onKeyPressEvent player event
    writeIORef playerRef player'
    return True

getNextStep :: Int -> Int -> Int
getNextStep currentStep steps
                | currentStep < (steps-1) = currentStep + 1
                | otherwise = 0

onKeyPressEvent :: Player -> QKeyEvent.QKeyEvent -> IO Player
onKeyPressEvent player event = do
    key <- QKeyEvent.key event
    doKeyAction player key

doKeyAction :: Player -> Int -> IO Player
doKeyAction player action
                | action == fromEnum Types.KeyDown = moveCharacterTo player KeyDown
                | action == fromEnum Types.KeyLeft = moveCharacterTo player KeyLeft
                | action == fromEnum Types.KeyRight = moveCharacterTo player KeyRight
                | action == fromEnum Types.KeyUp = moveCharacterTo player KeyUp
                | otherwise = do return player

moveCharacterTo :: Player -> Direction -> IO Player
moveCharacterTo player direction' = do

    let sprite' = sprite player
        stepSize = 4
        movement
                | (directionVal direction') == 0 = (0,stepSize)
                | (directionVal direction') == 1 = (negate stepSize,0)
                | (directionVal direction') == 2 = (stepSize,0)
                | (directionVal direction') == 3 = (0,negate stepSize)
                | otherwise = (0,0)

    let player' = Player {
        sprite = sprite',
        name = name (player :: Player),
        level = level player,
        xp = xp player,
        step = getNextStep (step player) (steps sprite'),
        direction = direction'
    }

    void(updatePixmapFromSprite ((step player') * (spriteStepWidth sprite')) ((directionVal direction') * (spriteStepHeight sprite')) (spriteStepWidth sprite') (spriteStepHeight sprite')  (spriteFile sprite') (pixmapItem sprite'))

    movePixmap (pixmapItem sprite') movement

    return player'

movePixmap :: QGraphicsPixmapItem.QGraphicsPixmapItem -> (Double,Double) -> IO ()
movePixmap pixmapItem movement = QGraphicsItem.moveBy pixmapItem (fst movement) (snd movement)

loadJson :: FromJSON a => FilePath -> IO (Maybe a)
loadJson filePath = do
                file <- BS.readFile filePath
                return $ decode file
