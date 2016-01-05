module Test.Lagrangian where

import ElmTest exposing (..)

import Lagrangian as Mech

all : Test
all =
  suite "Lagrangian" [
          ]


spec : Test
spec =
  let
    lagrangian = sum [ (square (velocity 0))
                     , (square (velocity 1))
                     , (times (num -1) (coordinate 1))
                     ]
                 
    acceleration = Mech.acceleration lagrangian
    initState = Mech.state1D 0 (0, 2)
    laterState = Mech.evolve 0.1 acceleration initState
  in
    assertEqual laterState initState



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
time : Int -> Expression

state1 : (Float, Float) -> State
state2 : (Float, Float) -> (Float, Float) -> State
state3 : (Float, Float) -> (Float, Float) -> (Float, Float) -> State
state : Float -> Array (Float, Float) -> State

findAcceleration : Expression -> Acceleration

evolve : Float -> Acceleration -> State -> State
