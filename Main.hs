{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (void)
import Control.Concurrent.Timer (oneShotTimer)
import Control.Concurrent.Suspend.Lifted (msDelay)

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
    scene :: QGraphicsScene.QGraphicsScene,
    player :: Player
}

data Player = Player {
    sprite :: Sprite,
    name :: String,
    level :: Int,
    xp :: Int,
    step :: Int,
    direction :: Direction
}

data Sprite = Sprite {
    spriteFile :: QPixmap.QPixmap,
    pixmapItem :: QGraphicsPixmapItem.QGraphicsPixmapItem,
    spriteStepWidth :: Int,
    spriteStepHeight :: Int,
    directions :: Int,
    steps :: Int
}

main :: IO ()
main = withScopedPtr (getArgs >>= QApplication.new) $ \_ -> do
  player <- initPlayer
  charRef <- newIORef player
  game <- initGame charRef
  QWidget.show game
  QCoreApplication.exec

initPlayer :: IO Player
initPlayer = do
        spriteFile <- QPixmap.newWithFile ("/home/cristian/main_char.png" :: String)
        (spriteWidth :: Int ) <- QPixmap.width spriteFile
        (spriteHeight :: Int ) <- QPixmap.height spriteFile
        pixmapItem <- QGraphicsPixmapItem.new

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

initGame :: IORef Player -> IO QWidget.QWidget
initGame playerRef = do
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

  QWidget.setWindowTitle widget ("Prueba" :: String)
  QWidget.resizeRaw widget 500 350
  QWidget.setLayout widget mainLayout

  QGraphicsScene.addItem gameScene $ pixmapItem (sprite player)

  _ <- onEvent widget $ \(event :: QKeyEvent.QKeyEvent) -> do
    isMoving <- readIORef movingRef
    if isMoving
      then return False
      else startMovement movingRef playerRef event

  return widget

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
            | otherwise = do
                            return player

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
        name = name player,
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
