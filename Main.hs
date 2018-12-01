{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (void)

import Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Gui.QPixmap as QPixmap
import qualified Graphics.UI.Qtah.Gui.QKeyEvent as QKeyEvent
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import qualified Graphics.UI.Qtah.Widgets.QGraphicsView as QGraphicsView
import qualified Graphics.UI.Qtah.Widgets.QGraphicsScene as QGraphicsScene
import qualified Graphics.UI.Qtah.Widgets.QGraphicsItem as QGraphicsItem
import qualified Graphics.UI.Qtah.Widgets.QGraphicsPixmapItem as QGraphicsPixmapItem
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Core.Types as Types
import Graphics.UI.Qtah.Signal (connect_)
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
    sprite :: IORef Sprite,
    name :: String,
    level :: Int,
    xp :: Int,
    step :: Int,
    direction :: Direction
}

data Sprite = Sprite {
    spriteFile :: QPixmap.QPixmap,
    pixmapItem :: IORef QGraphicsPixmapItem.QGraphicsPixmapItem,
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
        item <- QGraphicsPixmapItem.new
        pixmapItem' <- newIORef item

        let
          currentDirection = KeyDown
          currentStep = 0
          directions = 4
          steps = 3
          spriteStepWidth = quot spriteWidth steps
          spriteStepHeight = quot spriteHeight directions
          sprite = Sprite {
                spriteFile = spriteFile,
                pixmapItem = pixmapItem',
                spriteStepWidth = spriteStepWidth,
                spriteStepHeight = spriteStepHeight,
                directions = directions,
                steps = steps
          }

        sprite' <- newIORef sprite

        void(updatePixmapFromSprite (currentStep * spriteStepWidth) ((directionVal currentDirection) * spriteStepHeight) spriteStepWidth spriteStepHeight  spriteFile pixmapItem')

        return Player {
              sprite = sprite',
              direction = currentDirection,
              step = currentStep,
              name = "Test",
              level = 1,
              xp = 0
            }

updatePixmapFromSprite :: Int -> Int -> Int -> Int -> QPixmap.QPixmap -> IORef QGraphicsPixmapItem.QGraphicsPixmapItem -> IO ()
updatePixmapFromSprite x y width height sprite pixmapItemRef = do
        pixmapItem <- readIORef pixmapItemRef
        spriteFrame <- QPixmap.copyRaw sprite x y width height
        QGraphicsPixmapItem.setPixmap pixmapItem spriteFrame
        return ()

initGame :: IORef Player -> IO QWidget.QWidget
initGame caracterRef = do
  widget <- QWidget.new
  gameScene <- QGraphicsScene.new
  graphicsView <- QGraphicsView.newWithSceneAndParent gameScene widget
  mainLayout <- QVBoxLayout.new
  character <- readIORef caracterRef
  characterSprite <- readIORef $ sprite character
  pixmapItem' <- readIORef $ pixmapItem characterSprite

  QBoxLayout.addWidget mainLayout graphicsView

  QWidget.setWindowTitle widget ("Prueba" :: String)
  QWidget.resizeRaw widget 500 350
  QWidget.setLayout widget mainLayout

  QGraphicsScene.addItem gameScene pixmapItem'

  _ <- onEvent widget $ \(event :: QKeyEvent.QKeyEvent) -> do
    player <- onKeyPressEvent caracterRef event
    writeIORef caracterRef player
    return True

  return widget

getNextStep :: Int -> Int -> Int
getNextStep currentStep steps
                | currentStep < (steps-1) = currentStep + 1
                | otherwise = 0

onKeyPressEvent :: IORef Player -> QKeyEvent.QKeyEvent -> IO Player
onKeyPressEvent playerRef event = do
    player <- readIORef playerRef
    key <- QKeyEvent.key event

    doKeyAction playerRef key



doKeyAction :: IORef Player -> Int -> IO Player
doKeyAction playerRef action
            | (show action) == (show (Types.fromQtKeys Types.keyDown)) = moveCharacterTo playerRef KeyDown
            | (show action) == (show (Types.fromQtKeys Types.keyLeft)) = moveCharacterTo playerRef KeyLeft
            | (show action) == (show (Types.fromQtKeys Types.keyRight)) = moveCharacterTo playerRef KeyRight
            | (show action) == (show (Types.fromQtKeys Types.keyUp)) = moveCharacterTo playerRef KeyUp
            | otherwise = do
                            player <- readIORef playerRef
                            return player

moveCharacterTo :: IORef Player -> Direction -> IO Player
moveCharacterTo playerRef direction' = do
    player <- readIORef playerRef
    sprite' <- readIORef $ sprite player
    pixmapItem' <- readIORef $ pixmapItem sprite'
    let movement
                | (directionVal direction') == 0 = (0,2)
                | (directionVal direction') == 1 = (negate 2,0)
                | (directionVal direction') == 2 = (2,0)
                | (directionVal direction') == 3 = (0,negate 2)
                | otherwise = (0,0)

    let player' = Player {
      sprite = sprite player,
      name = name player,
      level = level player,
      xp = xp player,
      step = getNextStep (step player) (steps sprite'),
      direction = direction'
    }

    void(updatePixmapFromSprite ((step player') * (spriteStepWidth sprite')) ((directionVal direction') * (spriteStepHeight sprite')) (spriteStepWidth sprite') (spriteStepHeight sprite')  (spriteFile sprite') (pixmapItem sprite'))

    return player'
