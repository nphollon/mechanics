module Lagrangian (state1, state2, state3, state
                  , dimension, time, coordinate, velocity
                  , evolve, acceleration) where

import Array exposing (Array)
import TimeEvolution


state1 : (Float, Float) -> State
state1 x =
  state 0 [ x ]


state2 : (Float, Float) -> (Float, Float) -> State
state2 x y =
  state 0 [ x, y ]


state3 : (Float, Float) -> (Float, Float) -> (Float, Float) -> State
state3 x y z =
  state 0 [ x, y, z ]

        
state : Float -> List (Float, Float) -> State
state time coords =
  { time = time
  , coords = Array.fromList coords
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


evolve : Acceleration -> Float -> State -> State
evolve accel dt state =
  let
    tupPlus (a, b) (c, d) =
      (a + c, b + d)

    tupTimes f (a, b) =
      (f * a, f * b)

    tupForce a (x, v) =
      (v, a)

    add s1 s2 =
      { s1 | coords =
             List.map2 tupPlus (Array.toList s1.coords) (Array.toList s2.coords)
               |> Array.fromList
      }

    scale f s =
      { s | coords =
             Array.map (tupTimes f) s.coords
      }

    force s =
      { s | coords =
            List.map2 tupForce (accel s) (Array.toList s.coords)
                |> Array.fromList
      }

    laws =
      { add = add
      , scale = scale
      , force = force
      }
  in
    TimeEvolution.evolve laws (1000 * dt) state
                 

acceleration : (State -> List Float) -> Acceleration
acceleration a =
  a

  
type alias State =
  { time : Float
  , coords : Array (Float, Float)
  }

                 
type alias Acceleration =
  State -> List Float
