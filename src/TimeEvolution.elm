module TimeEvolution (Laws, evolve) where

{-| Evolve a system continuously through time. This module does not know any
physics. You provide it the physical laws, and it updates your state for you.

`evolve` uses the [Runge-Kutta method](https://en.wikipedia.org/wiki/Runge-Kutta_methods),
which is much more accurate than the simpler
[Euler method](https://en.wikipedia.org/wiki/Euler_method).

@docs Laws, evolve
-}

import Time exposing (Time)


{-| Laws describes the behavior of the system.

The model contains all of the data that needs to be updated. The values will
change continuously over time, so this should be a collection of `Float`s.
For example, the model might include things like position and velocity. 

* add: Add the corresponding elements of one model to another
* scale: Multiply all the elements of a model by a factor
* force: Given a model, return the time derivative of the model

Note that `force` is not the same as the update function you normally pass to
`Signal.foldp`. It does not return an updated model, it returns a derivative.

    type alias State = { position : Float, momentum : Float }

    mass = 0.5
    gravity = 9.81

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
          { position = state.momentum / mass -- Change in position
          , momentum = gravity -- Change in momentum
          }
-}
type alias Laws model =
  { add : model -> model -> model
  , scale : Float -> model -> model
  , force : model -> model
  }

                                    
{-| Given a set of laws, a time delta, and an initial state, compute the state
at the new time. This is suitable as the update function for a `Signal.foldp`

    initialState = { position = 0, momentum = 1 }

    system = Signal.foldp (evolve laws) initialState (Time.fps 60)
-}                      
evolve : Laws a -> Time -> a -> a
evolve { add, scale, force } dt phase =
  let
    integrate =
      Time.inSeconds >> scale
          
    a = force phase
    b = a |> integrate (0.5 * dt) |> add phase |> force
    c = b |> integrate (0.5 * dt) |> add phase |> force
    d = c |> integrate dt |> add phase |> force
  in
    add a d
      |> add (scale 2 b)
      |> add (scale 2 c)
      |> integrate (dt / 6)
      |> add phase
