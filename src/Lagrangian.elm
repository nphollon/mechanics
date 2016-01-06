module Lagrangian (state1, state2, state3, state
                  , dimension, time, coordinate, velocity
                  , evolve, acceleration) where

import Array exposing (Array)


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
    
    a = force accel state
    b = a |> scale (0.5 * dt) |> add state |> force accel
    c = b |> scale (0.5 * dt) |> add state |> force accel
    d = c |> scale dt |> add state |> force accel
  in
    add a d
      |> add (scale 2 b)
      |> add (scale 2 c)
      |> scale (dt / 6)
      |> add state


add : State -> State -> State
add s1 s2 =
  let
    tupPlus (a, b) (c, d) =
      (a + c, b + d)
  in
    { s1 | coords =
           List.map2 tupPlus (Array.toList s1.coords) (Array.toList s2.coords)
             |> Array.fromList
    }


scale : Float -> State -> State
scale f s =
  let
    tupTimes f (a, b) =
      (f * a, f * b)
  in
    { s | coords =
          Array.map (tupTimes f) s.coords
    }


force : Acceleration -> State -> State
force accel s =
  let
    tupForce a (x, v) =
      (v, a)
  in
    { s | coords =
          List.map2 tupForce (accel s) (Array.toList s.coords)
            |> Array.fromList
    }
  

acceleration : (State -> List Float) -> Acceleration
acceleration a =
  a

  
type alias State =
  { time : Float
  , coords : Array (Float, Float)
  }

                 
type alias Acceleration =
  State -> List Float
