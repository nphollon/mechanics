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
         (Mech.state 0 [(1, 2)])

  , test "states are unequal if their coordinates differ"
    <| assertNotEqual
         (Mech.state1 (1, 2))
         (Mech.state 0 [(2, 2)])

  , test "states are unequal if their times differ"
    <| assertNotEqual
         (Mech.state1 (1, 2))
         (Mech.state 0.5 [(1, 2)])

  , test "state2 is sugar for 2-dimensional state"
    <| assertEqual
         (Mech.state2 (4, 2) (3, 1))
         (Mech.state 0 [(4, 2), (3, 1)])
         
  , test "state3 is sugar for 3-dimensional state"
    <| assertEqual
         (Mech.state3 (8, 9) (4, 2) (3, 1))
         (Mech.state 0 [(8, 9), (4, 2), (3, 1)])

  , test "dimension returns number of coordinates"
    <| assertEqual
         2
         (Mech.dimension (Mech.state2 (0, 0) (0, 0)))

  , test "time returns time"
    <| assertEqual
         1.5
         (Mech.time (Mech.state 1.5 []))

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
  let
    inert =
      Mech.acceleration (always [ 0 ])

    falling =
      Mech.acceleration (always [ -2 ])
  in
  [ test "state does not change if acceleration and velocity are zero"
    <| assertEqual
         (Mech.state1 (1, 0))
         (Mech.evolve inert 1.0 (Mech.state1 (1, 0)))

  , test "position changes if velocity is non-zero"
    <| assertEqual
         (Mech.state1 (2, 1))
         (Mech.evolve inert 1.0 (Mech.state1 (1, 1)))

  , test "dx = dt * v"
    <| assertEqual
         (Mech.state1 (1.5, 1))
         (Mech.evolve inert 0.5 (Mech.state1 (1, 1)))

  , test "dv = dt * a"
    <| assertEqual
         (Mech.state1 (-0.25, -1))
         (Mech.evolve falling 0.5 (Mech.state1 (0, 0)))
  ]
  
{-
lagrangianToAcceleration : Expression -> Acceleration

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
