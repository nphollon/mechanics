module LagrangianTest (all) where

import ElmTest exposing (..)
import Lagrangian exposing (..)
import Expression exposing (..)
import Mechanics exposing (State)


all : Test
all =
    suite
        "Lagrangian mechanics"
        [ suite "Solving Lagrange equations" solveTests
        , suite "Getting accelerations from lagrangians" accelerationTests
        ]



{-
lagrangianToAcceleration : Expression -> Acceleration
-}


solveTests : List Test
solveTests =
    [ test "1D free particle lagrangian"
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
    , test "Cannot solve 2D Lagrangian with velocity cross-derivatives"
        <| assertNoAcceleration
            ((square (velocity 0)) `times` (square (velocity 1)))
    ]


assertAcceleration : List Expression -> Expression -> Assertion
assertAcceleration expected lagr =
    assertEqual (Just expected) (solve lagr)


assertNoAcceleration : Expression -> Assertion
assertNoAcceleration lagr =
    assertEqual Nothing (solve lagr)


accelerationTests : List Test
accelerationTests =
    let
        falling =
            (((num 0.5) `times` ((square (velocity 0)) `plus` (square (velocity 1))))
                `minus` ((num 10) `times` (coordinate 1))
            )
                |> toAcceleration
                |> Maybe.withDefault (Mechanics.acceleration (always []))
    in
        [ test "gravitational potential"
            <| assertAboutEqual
                (Mechanics.state 0.5 [ ( 3, 6 ), ( -1.25, -5 ) ])
                (Mechanics.evolve falling 0.5 (Mechanics.state2 ( 0, 6 ) ( 0, 0 )))
        ]


assertAboutEqual : State -> State -> Assertion
assertAboutEqual a b =
    if Mechanics.aboutEqual 1.0e-10 a b then
        assert True
    else
        assertEqual a b
