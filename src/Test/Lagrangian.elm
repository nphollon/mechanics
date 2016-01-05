module Test.Lagrangian where

import Array

import ElmTest exposing (..)

import Lagrangian as Mech

all : Test
all =
  suite "Dynamics"
          [ suite "Building states" stateTests
          , suite "Evolving states" accelerationTests
          ]


stateTests : List Test
stateTests =
  [ test "state1 is sugar for 1-dimensional state"
    <| assertEqual
         (Mech.state1 (1, 2))
         (Mech.state 0 (Array.fromList [(1, 2)]))

  , test "states are unequal if their coordinates differ"
    <| assertNotEqual
         (Mech.state1 (1, 2))
         (Mech.state 0 (Array.fromList [(2, 2)]))

  , test "states are unequal if their times differ"
    <| assertNotEqual
         (Mech.state1 (1, 2))
         (Mech.state 0.5 (Array.fromList [(1, 2)]))

  , test "state2 is sugar for 2-dimensional state"
    <| assertEqual
         (Mech.state2 (4, 2) (3, 1))
         (Mech.state 0 (Array.fromList [(4, 2), (3, 1)]))
         
  , test "state3 is sugar for 3-dimensional state"
    <| assertEqual
         (Mech.state3 (8, 9) (4, 2) (3, 1))
         (Mech.state 0 (Array.fromList [(8, 9), (4, 2), (3, 1)]))

  , test "dimension returns number of coordinates"
    <| assertEqual
         2
         (Mech.dimension (Mech.state2 (0, 0) (0, 0)))

  , test "time returns time"
    <| assertEqual
         1.5
         (Mech.time (Mech.state 1.5 Array.empty))

  , test "coordinate returns coordinate with given index"
    <| assertEqual
         5
         (Mech.coordinate 1 (Mech.state2 (0, 0) (5, 0)))

  , test "coordinate returns 0 if index is out of range"
    <| assertEqual
         0
         (Mech.coordinate 2 (Mech.state2 (0, 0) (5, 0)))

  , test "velocity returns velocity with given index"
    <| assertEqual
         7
         (Mech.velocity 1 (Mech.state2 (0, 0) (0, 7)))
  ]


accelerationTests : List Test
accelerationTests =
  []
  
{-
state1 : (Float, Float) -> State
state2 : (Float, Float) -> (Float, Float) -> State
state3 : (Float, Float) -> (Float, Float) -> (Float, Float) -> State
state : Float -> Array (Float, Float) -> State

velocity : Int -> State -> Float
coordinate : Int -> State -> Float
time : State -> Float
arity : State -> Int

lagrangianToAcceleration : Expression -> Acceleration

acceleration : (State -> State) -> Acceleration

evolve : Float -> Acceleration -> State -> State
-}
{-
num : Float -> Expression
plus : Expression -> Expression -> Expression
minus : Expression -> Expression -> Expression
over : Expression -> Expression -> Expression
times : Expression -> Expression -> Expression
square : Expression -> Expression
expt : Expression -> Expression -> Expression
sin : Expression -> Expression
cos : Expression -> Expression
log : Expression -> Expression

sum : List Expression -> Expression
product : List Expression -> Expression

velocity : Int -> Expression
coordinate : Int -> Expression
time : Expression

eval : Expression -> State -> State
-}
