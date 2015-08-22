module KeyboardPresses (presses) where

-- implement workaround for Keyboard.presses

import Keyboard exposing (KeyCode, keysDown)
import Set exposing (Set, empty)
import Signal exposing (foldp, (<~), (~))

type alias KeyboardState = { temp : Set KeyCode
                           , actualKey : Maybe KeyCode
                           }
keyboardState = { temp = empty
                , actualKey = Nothing
                }

step : Set KeyCode -> KeyboardState -> KeyboardState
step newKeys state =
  let
    actualKey = List.head <| Set.toList <| newKeys `Set.diff` state.temp
  in
    { state |
      temp <- newKeys
    , actualKey <- actualKey
    }

presses : Signal (Maybe KeyCode)
presses = .actualKey
          <~ Signal.foldp step keyboardState Keyboard.keysDown
