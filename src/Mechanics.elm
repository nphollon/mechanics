module Mechanics (State, Acceleration
                 , state1, state2, state3, state
                 , aboutEqual, dimension, time, coordinate, velocity
                 , evolve, acceleration) where

{-|
# Building states
@docs State, state, state1, state2, state3

# Inspecting states
@docs aboutEqual, dimension, time, coordinate, velocity

# Changing states
@docs Acceleration, acceleration, evolve
-}
import Array exposing (Array)


-- Building states

{-| A state describes a physical system at a moment in time. It contains three types of numbers:

* Time.
* Coordinates describing the position of the system. If you took a snapshot of the system, what would it look like?
* Velocities describing the rates of change for each coordinate. If you took another snapshot a split-second later, how much would it have changed?

Coordinates and velocities do not need to be rectangular (in X-Y-Z space). For example, a satellite orbiting the Earth could be described by spherical coordinates (altitude, latitude, and longitude). The coordinates and velocities don't even need to be spatial positions. For example, a kettle of water could have a coordinate for temperature. The "velocity" would be the rate of temperature change.
-}

type State =
  Data 
  { time : Float
  , coordinates : List Float
  , velocities : List Float
  }


{-| Create a state with the given time, coordinates, and velocities. Because coordinates and velocities correspond, they are given as a list of pairs.

    t = 10.0 -- seconds
    x = 0.0 -- meters
    y = 10.0 -- meters
    xSpeed = 0.0 -- meters per second
    ySpeed = -1.0 -- meters per second

    state = t [ (x, xSpeed), (y, ySpeed) ]
-}
state : Float -> List (Float, Float) -> State
state time coords =
  { time = time
  , coordinates = List.map fst coords
  , velocities = List.map snd coords
  } |> Data


{-| Create a 1-dimensional state. The time is set to zero.

    state1 (x, v) -- equals state 0 [ (x, v) ]
-}
state1 : (Float, Float) -> State
state1 x =
  state 0 [ x ]


{-| Create a 2-dimensional state. The time is set to zero.

    state2 (x, vx) (y, vy) 
    -- equals state 0 [ (x, vx), (y, vy) ]
-}
state2 : (Float, Float) -> (Float, Float) -> State
state2 x y =
  state 0 [ x, y ]


{-| Create a 3-dimensional state. The time is set to zero.

    state2 (x, vx) (y, vy) (z, vz)
    -- equals state 0 [ (x, vx), (y, vy), (z, vz) ]
-}
state3 : (Float, Float) -> (Float, Float) -> (Float, Float) -> State
state3 x y z =
  state 0 [ x, y, z ]


-- Inspecting states

{-| 
    aboutEqual tolerance a b

Compares all numbers in state A and state B. Returns `True` if they differ by
less than the given tolerance. States with different dimensions are never equal.

    aboutEqual 1e-3 (state1 (0, 0)) (state1 (0, 1e-4)) -- returns True

    aboutEqual 1e-6 (state1 (0, 0)) (state1 (0, 1e-4)) -- returns False

    aboutEqual 1e-6 (state1 (0, 0)) (state2 (0, 0) (0, 0)) -- returns False
-}
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
    
  
{-| Returns the number of coordinates in a state. 
    
    dimension (state1 (0, 0)) -- returns 1
    dimension (state3 (0, 0) (0, 0) (0, 0)) -- returns 3
-}
dimension : State -> Int
dimension (Data state) =
  List.length state.coordinates


{-| Returns the time of a state.

    time (state1 (1, 5)) -- returns 0
    time (state 3.5 [ (0, 0) ]) -- returns 3.5
-}
time : State -> Float
time (Data state) =
  state.time


{-| For a given index `n`, returns the `n`th coordinate of a state. This works
similarly to `Array.get`. The index is zero-based. An out-of-bounds index returns
zero.

    theState = state3 (1, 2) (3, 4) (5, 6)

    coordinate 0 theState -- returns 1
    coordinate 2 theState -- returns 5
    coordinate 3 theState -- returns 0
    coordinate -1 theState -- returns 0
-}
coordinate : Int -> State -> Float
coordinate i (Data state) =
  Array.fromList state.coordinates
    |> Array.get i
    |> Maybe.withDefault 0


{-| Returns the `n`th velocity of a state.

    theState = state3 (1, 2) (3, 4) (5, 6)

    velocity 0 theState -- returns 2
    velocity 2 theState -- returns 6
    velocity 3 theState -- returns 0

-}
velocity : Int -> State -> Float
velocity i (Data state) =
  Array.fromList state.velocities
    |> Array.get i 
    |> Maybe.withDefault 0


-- Evolving states

{-| An acceleration describes how the velocities of a state change with time.-}
type Acceleration =
  Force (State -> List Float)


{-| Create an acceleration from a function. The function takes a state and
returns a list of changes to the velocities.

Example 1: An object in freefall. The state has 2 dimensions, X and Y.

    gravity = -10
    fallingAccel = acceleration (always [0, gravity])

Example 2: A weight attached to a spring. The weight is pushed/pulled towards
the resting position of the spring. The state has 1 dimension, X.

    springStrength = 2.0
    restPosition = 5.0
    mass = 1.0    

    hookesLaw position =
      mass * springStrength * (restPosition - position)

    springAccel = acceleration (\s -> [ hookesLaw (coordinate 0 s) ])
-}
acceleration : (State -> List Float) -> Acceleration
acceleration a =
  Force a

  
{-| Given an acceleration, a change in time, and a state, evolve the state
forward in time. (Under the hood, `evolve` uses the
[Runge-Kutta method](https://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods).)

    start = state2 (0, 1) (10, 0)

    oneSecondLater = evolve fallingAccel 1.0 start 
    -- returns state 0.5 [ (1, 1) (5, -10) ]

Toss this sucker into a `foldp`, and watch the Universe come to life before
your eyes!

    model = Signal.foldp (evolve fallingAccel) start (Time.fps 30)
-}
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
