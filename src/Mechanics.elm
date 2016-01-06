module Mechanics (State, Acceleration
                 , state1, state2, state3, state
                 , aboutEqual, dimension, time, coordinate, velocity
                 , evolve, acceleration) where

{-|
# Building states
@docs State, state1, state2, state3, state

# Inspecting states
@docs aboutEqual, dimension, time, coordinate, velocity

# Changing states
@docs Acceleration, acceleration, evolve
-}
import Array exposing (Array)


-- Building states

{-|-}
type State =
  Data 
  { time : Float
  , coordinates : List Float
  , velocities : List Float
  }

           
{-|-}
state1 : (Float, Float) -> State
state1 x =
  state 0 [ x ]


{-|-}
state2 : (Float, Float) -> (Float, Float) -> State
state2 x y =
  state 0 [ x, y ]


{-|-}
state3 : (Float, Float) -> (Float, Float) -> (Float, Float) -> State
state3 x y z =
  state 0 [ x, y, z ]

        
{-|-}
state : Float -> List (Float, Float) -> State
state time coords =
  { time = time
  , coordinates = List.map fst coords
  , velocities = List.map snd coords
  } |> Data


-- Inspecting states

{-|-}
aboutEqual : Float -> State -> State -> Bool
aboutEqual tolerance (Data a) (Data b) =
  let
    eq x y =
      (x - y)^2 < tolerance^2

    eqAll xs ys =
      (List.length xs == List.length ys) &&
      (List.map2 eq xs ys |> List.all identity)
  in
    (eq a.time b.time) &&
    (eqAll a.coordinates b.coordinates) &&
    (eqAll a.velocities b.velocities)
    
  
{-|-}
dimension : State -> Int
dimension (Data state) =
  List.length state.coordinates


{-|-}
time : State -> Float
time (Data state) =
  state.time


{-|-}
coordinate : Int -> State -> Float
coordinate i (Data state) =
  Array.fromList state.coordinates
    |> Array.get i
    |> Maybe.withDefault 0


{-|-}
velocity : Int -> State -> Float
velocity i (Data state) =
  Array.fromList state.velocities
    |> Array.get i 
    |> Maybe.withDefault 0


-- Evolving states

{-|-}
type Acceleration =
  Force (State -> List Float)


{-|-}
acceleration : (State -> List Float) -> Acceleration
acceleration a =
  Force a

  
{-|-}
evolve : Acceleration -> Float -> State -> State
evolve accel dt state =
  let
    a = stateDerivative accel state
    b = nudge (0.5 * dt) a state |> stateDerivative accel
    c = nudge (0.5 * dt) b state |> stateDerivative accel
    d = nudge dt c state |> stateDerivative accel
  in
    state
      |> nudge (dt / 6) a
      |> nudge (dt / 3) b
      |> nudge (dt / 3) c
      |> nudge (dt / 6) d


nudge : Float -> State -> State -> State
nudge dt (Data derivative) (Data state) =
  let
    add dxdt x =
      x + dt * dxdt

    combine getter =
      List.map2 add (getter derivative) (getter state)
  in
    { time = state.time + dt
    , coordinates = combine .coordinates
    , velocities = combine .velocities
    } |> Data
      

stateDerivative : Acceleration -> State -> State
stateDerivative (Force accel) (Data state) =
  { state
    | coordinates = state.velocities
    , velocities = accel (Data state)
  } |> Data
