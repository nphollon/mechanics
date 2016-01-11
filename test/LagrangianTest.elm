module LagrangianTest (all) where

import ElmTest exposing (..)
import Lagrangian exposing (..)
import Expression exposing (..)


all : Test
all =
    suite
        "Lagrangian mechanics"
        [ suite "Solving Lagrange equations" lagrangeTests
        ]



{-
solveLagrangian : Expression -> Maybe (List Expression)
lagrangianToAcceleration : Expression -> Acceleration
-}


lagrangeTests : List Test
lagrangeTests =
    [ test "Lagrangian with velocity 0 is 1-dimensional"
        <| assertEqual
            1
            (dimension (velocity 0))
    , test "Lagrangian with velocity 1 is 2-dimensional"
        <| assertEqual
            2
            (dimension ((velocity 0) `plus` (velocity 1)))
    , test "Lagrangian with coordinate 2 is 3-dimensional"
        <| assertEqual
            3
            (dimension ((velocity 0) `times` (coordinate 2)))
    , test "1D free particle lagrangian"
        <| assertAcceleration
            [ num 0 ]
            (square (velocity 0))
    , test "1D Lagrangian in gravity well"
        <| assertAcceleration
            [ num -0.5 ]
            ((square (velocity 0)) `minus` (coordinate 0))
    , test "1D Lagrangian with time-dependent kinetic energy"
        <| assertAcceleration
            [ product [ num -1, velocity 0, expt time (num -1) ] ]
            ((square (velocity 0)) `times` time)
    , test "1D Lagrangian with coordinate-dependent kinetic energy"
        <| assertAcceleration
            [ product [ num -0.5, expt (velocity 0) (num 2), expt (coordinate 0) (num -1) ] ]
            (product [ coordinate 0, expt (velocity 0) (num 2) ])
    , test "Cannot solve 1D Lagrangian with no square-velocity term"
        <| assertNoAcceleration
            (velocity 0)
    , test "2D Lagrangian in gravity well"
        <| assertAcceleration
            [ num 0, num -0.5 ]
            (sum [ square (velocity 0), square (velocity 1), (num -1) `times` (coordinate 1) ])
    , test "2D Lagrangian with coordinate-dependent velocity"
        <| assertAcceleration
            [ product [ num -1, velocity 1, velocity 0, expt (coordinate 1) (num -1) ]
            , (num 0.5) `times` (square (velocity 0))
            ]
            (((square (velocity 0)) `times` (coordinate 1)) `plus` (square (velocity 1)))
    ]


assertAcceleration : List Expression -> Expression -> Assertion
assertAcceleration expected lagr =
    assertEqual (Just expected) (solveLagrangian lagr)


assertNoAcceleration : Expression -> Assertion
assertNoAcceleration lagr =
    assertEqual Nothing (solveLagrangian lagr)
