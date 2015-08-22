module Roguelike where

import Color exposing (Color)
import Graphics.Collage as C
import Graphics.Element as E exposing (Element)
import Keyboard
import Signal as S exposing (Signal, (<~), (~))
import Text
import Window

-- my modules
import KeyboardPresses
import Matrix exposing (Matrix)

-- CONFIG
screenWidth : Int -- maybe screen is not the correct name
screenWidth = 128
screenHeight : Int
screenHeight = 80

-- -- DEBUG
-- screenWidth : Int -- maybe screen is not the correct name
-- screenWidth = 8
-- screenHeight : Int
-- screenHeight = 4


-- MODEL
type alias Game = { player : Object
                  , enemies : List Object
                  , tileMap : TileMap
                  , gameMap : GameMap -- gameMap is needed for view
                  }
type alias Object = { x : Int
                    , y : Int
                    , char : String -- one-character string
                    , color : Color
                    }
type alias Tile = { blocked : Bool
                  , blockSight : Bool
                  , colorDark : Color
                  , colorLight : Color
                  }
type alias TileMap = Matrix Tile
type alias GameMap = Matrix (Tile, List Object)

defaultGame : Game
defaultGame = { player = defaultPlayer
              , enemies = [defaultEnemy]
              , tileMap = currentTileMap
              , gameMap = defaultGameMap
              }

defaultPlayer : Object
defaultPlayer = { x = 0
                , y = 0
                , char = "@"
                , color = Color.white
                }

defaultEnemy : Object
defaultEnemy = { x = screenWidth//2  // 2
               , y = screenHeight//2
               , char = "@"
               , color = Color.yellow
               }

colorDarkWall = Color.rgb 0 0 100
colorDarkGround = Color.rgb 50 50 150
colorLightWall = Color.rgb 130 110 50
colorLightGround = Color.rgb 200 180 50

ground : Tile
ground = { blocked = False
         , blockSight = False
         , colorDark = colorDarkGround
         , colorLight = colorLightGround
         }
wall : Tile
wall = { blocked = True
       , blockSight = True
       , colorDark = colorDarkWall
       , colorLight = colorLightWall
       }

defaultTileMap : TileMap
defaultTileMap = Matrix.repeat screenWidth screenHeight ground

currentTileMap : TileMap
currentTileMap = Matrix.set 20 22 wall defaultTileMap
           |> Matrix.set 40 22 wall

defaultGameMap : GameMap
defaultGameMap = Matrix.repeat screenWidth screenHeight (ground, [])

-- UPDATE
-- at some point I will change that to Event -> Game -> Game
stepGame : Maybe Keyboard.KeyCode -> Game -> Game
stepGame key game =
  case key of
    Nothing -> game
    Just key ->
      -- 37,38,39,40 ~ left,up,right,down
      if | key == 37 -> { game | player <- move -1  0 game.player }
         | key == 38 -> { game | player <- move  0 -1 game.player }
         | key == 39 -> { game | player <- move  1  0 game.player }
         | key == 40 -> { game | player <- move  0  1 game.player }
         | otherwise -> game

-- move object from initial position by dx, dy
move : Int -> Int -> Object -> Object
move x y object =
  { object | x <- object.x + x, y <- object.y + y }


-- recalibrate collage (center to left-upper)
-- x, y - cell index
moveCollage : (Int, Int) -> Bool -> C.Form -> C.Form
moveCollage (x, y) isText form =
  let
    sw = toFloat screenWidth
    sh = toFloat screenHeight
    -- C.text doesn't place text vertically in the middle, it places
    -- it a little bit lower than that. Because I can't use containers
    -- for performance reasons, I have to manually nudge the character
    -- so that it is almost exactly in the middle. I've found that
    -- coefficient of calibration is 7/32 (but this might not be
    -- entirely precise).
    verticalCalibration = if isText then 7/32 else 0
    newX = -sw/2 + (toFloat x) + 1/2
    newY =  sh/2 - (toFloat y) - 1/2 - verticalCalibration
  in
    C.move (newX, newY) form


drawTile : (Int, Int, Tile) -> C.Form
drawTile (x,y,tile) =
  C.rect 1 1
    |> C.filled tile.colorDark
    |> moveCollage (x,y) False


drawObject : Object -> C.Form
drawObject object =
  let
    charSize = 9/10
  in
    object.char
      |> Text.fromString
      |> Text.height charSize
      |> Text.color object.color
      |> C.text
      |> moveCollage (object.x, object.y) True


collageSize : (Int, Int) -> (Float, Int, Int)
collageSize (ww', wh') =
  let
    -- Int->Float conversions allow better precision arithmetic
    -- let x = toFloat x is illegal in Elm,
    -- so arguments are with apostrophes.
    -- ww - window width, sh - screen height, cw - collage width
    ww = toFloat ww'
    wh = toFloat wh'
    sw = toFloat screenWidth
    sh = toFloat screenHeight
    windowScale = ww / wh
    screenScale = sw / sh
    -- wh = sh * C
    -- ww = sw * C
    -- if wh, sh, sw are known,
    -- then C = wh/sh, ww = sw*C, w = sw*wh/s
    scaledW = wh * sw / sh
    scaledH = ww * sh / sw
    scale = if windowScale > screenScale
            then scaledW / sw
            else scaledH / sh
  in
    (scale, round scaledW, round scaledH)


view : (Int, Int) -> Game -> Element
view (ww,wh) game =
  let
    (scale, cw, ch) = collageSize (ww,wh)
    player = game.player

    playerForm = drawObject player
    enemyForms = List.map drawObject game.enemies
    indexedTiles = Matrix.toIndexedList game.tileMap
    tileForms = List.map drawTile indexedTiles
    -- tileForms = []

    -- scaling
    formsGroup = C.scale scale
                 <| C.group
                 <| tileForms++(playerForm::enemyForms)
                 -- <| playerForm::enemyForms
    forms = [ formsGroup ]
  in
    E.color Color.lightGray
       <| E.container ww wh E.middle
            <| E.color Color.black
                 <| C.collage cw ch forms


-- MAIN
main : Signal Element
main = view
       <~ Window.dimensions
       ~ Signal.foldp stepGame defaultGame KeyboardPresses.presses
