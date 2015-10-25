module TimeEvolution (Laws, Externals, limit, evolve) where

{-| Evolve a system continuously through time. This module does not know any
physics. You provide it the physical laws, and it updates your state for you.

`evolve` uses the [Runge-Kutta method](https://en.wikipedia.org/wiki/Runge-Kutta_methods),
which is much more accurate than the simpler
[Euler method](https://en.wikipedia.org/wiki/Euler_method).

@docs Laws, Externals, limit, evolve

-}

import List
import Signal exposing (Signal)
import Time exposing (Time)


{-| Laws describes the behavior of the system.

The model contains all of the data that needs to be updated. The values will
change continuously over time, so this should be a collection of `Float`s.
For example, the model might include things like position and velocity. 

The environment contains information that is not controlled by `evolve`.
It might include constants (such as gravitational strength) or data
controlled by the user (such as acceleration).

* add: Add the corresponding elements of one model to another
* scale: Multiply all the elements of a model by a factor
* force: Given an environment and a model, return the time derivative of the 
model

Note that `force` is not the same as the update function you normally pass to
`Signal.foldp`. It does not return an updated model, it returns a derivative.

    type alias State = { position : Float, momentum : Float }
    type alias Env = { force : Float, mass : Float }

    laws =
      { add a b = 
          { position = a.position + b.position
          , momentum = a.momentum + b.momentum
          }
      , scale f state =
          { position = f * state.position
          , momentum = f * state.momentum
          }
      , force env state =
          { position = state.momentum / env.mass -- Change in position
          , momentum = state.force -- Change in momentum
          }
-}
type alias Laws model environment =
  { add : model -> model -> model
  , scale : Float -> model -> model
  , force : environment -> model -> model
  }


{-| Externals contains signals for time deltas and environment updates.

    externals =
      { deltas = Time.fps 60
      , env = Signal.constant { force = 1, mass = 5 }
      }
-}
type alias Externals environment =
  { deltas : Signal Time
  , env: Signal environment
  }

type alias Update a =
  (Time, a)

                                    
{-| Given a set of laws and an initial state, send a state update every time
externals are updated. (Compare to `Signal.foldp`)

    state = evolve laws { position = 0, momentum = 1 } externals
-}
evolve : Laws a b -> a -> Externals b -> Signal a
evolve config init extern =
  let
    transform =
      rungeKutta config

    update =
      Signal.map2 (,) extern.deltas extern.env
  in
    Signal.foldp transform init update


rungeKutta : Laws a b -> Update b -> a -> a
rungeKutta { add, scale, force } (dt, env) phase =
  let
    integrate =
      Time.inSeconds >> scale
          
    a = force env phase
    b = a |> integrate (0.5 * dt) |> add phase |> force env
    c = b |> integrate (0.5 * dt) |> add phase |> force env
    d = c |> integrate dt |> add phase |> force env
  in
    add a d
      |> add (scale 2 b)
      |> add (scale 2 c)
      |> integrate (dt / 6)
      |> add phase

         
{-| Ignore time deltas larger than a maximum value. Changing focus to another
tab or window can cause large time deltas, which break the simulation.

    -- Drop deltas greater than a quarter-second
    timeLimited = limit (0.25 * Time.second) externals
-}
limit : Time -> Externals a -> Externals a
limit maxDelta extern =
  let
    limited =
      Signal.filter (\delta -> delta < maxDelta) 0 extern.deltas
  in
    { extern | deltas <- limited }
