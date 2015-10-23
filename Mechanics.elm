module Mechanics (Operators, External, external, evolve, operators) where

{-|

@docs Operators, External, external, evolve, operators

-}

import List
import Signal exposing (Signal, (<~), (~))
import Time exposing (Time)

{-|-}
evolve : Operators a -> (b -> a -> a) -> a -> External b -> Signal a
evolve operators force initialState (Evolving external) =
  let
    transform = rungeKutta operators force
  in
    Signal.foldp transform initialState external


rungeKutta : Operators a -> (b -> a -> a) -> Update b -> a -> a
rungeKutta (Normal { add, scale }) force { env, dt }  phase =
  {- Changing focus to another tab or window can cause large
     time deltas, which break the simulation, so we will
     ignore time deltas larger than a quarter second -}
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
         

{-|-}
operators : (a -> a -> a) -> (Float -> a -> a) -> Operators a
operators add scale =
  Normal { add = add, scale = scale }


{-|-}
external : Float -> Signal Time -> Signal a -> External a
external timeLimit time envSignal =
  let
    update env dt =
      { env = env, dt = dt }

    isSmall dt =
      dt < timeLimit

  in
    Signal.filter isSmall 0 time
      |> Signal.map2 update envSignal
      |> Evolving
         
         
{-|-}
type Operators a =
  Normal
    { add : (a -> a -> a)
    , scale : (Float -> a -> a)
    }

  
{-|-}
type External a =
  Evolving (Signal (Update a))

type alias Update a =
  { env: a
  , dt: Time
  }
