module Lagrangian (state1, state2, state3, state
                  , dimension, time, coordinate, velocity
                  ) where

import Array exposing (Array)


state1 : (Float, Float) -> State
state1 coord =
  state 0 (Array.repeat 1 coord)


state2 : (Float, Float) -> (Float, Float) -> State
state2 x y =
  state 0 (Array.fromList [ x, y ])


state3 : (Float, Float) -> (Float, Float) -> (Float, Float) -> State
state3 x y z =
  state 0 (Array.fromList [ x, y, z ])

        
state : Float -> Array (Float, Float) -> State
state time coords =
  { time = time
  , coords = coords
  }


dimension : State -> Int
dimension state =
  Array.length state.coords


time : State -> Float
time =
  .time


coordinate : Int -> State -> Float
coordinate i state =
  Array.get i state.coords
    |> Maybe.map fst
    |> Maybe.withDefault 0


velocity : Int -> State -> Float
velocity i state =
  Array.get i state.coords
    |> Maybe.map snd
    |> Maybe.withDefault 0

            
type alias State =
  { time : Float
  , coords : Array (Float, Float)
  }
