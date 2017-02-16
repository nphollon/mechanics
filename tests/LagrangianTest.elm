module LagrangianTest exposing (all)

import Expect exposing (Expectation)
import Test exposing (..)
import Lagrangian exposing (..)
import Expression exposing (..)
import Mechanics exposing (State)


all : Test
all =
    describe
        "Lagrangian mechanics"
        [ describe "Solving Lagrange equations" solveTests
        , describe "Getting accelerations from lagrangians" accelerationTests
        ]



{-
   lagrangianToAcceleration : Expression -> Acceleration
-}


solveTests : List Test
solveTests =
    [ test "1D free particle lagrangian" <|
        \() ->
            assertAcceleration
                [ num 0 ]
                (square (velocity 0))
    , test "1D Lagrangian in gravity well" <|
        \() ->
            assertAcceleration
                [ num -0.5 ]
                (minus (square (velocity 0)) (coordinate 0))
    , test "1D Lagrangian with time-dependent kinetic energy" <|
        \() ->
            assertAcceleration
                [ product [ num -1, velocity 0, expt time (num -1) ] ]
                (times (square (velocity 0)) time)
    , test "1D Lagrangian with coordinate-dependent kinetic energy" <|
        \() ->
            assertAcceleration
                [ product [ num -0.5, expt (velocity 0) (num 2), expt (coordinate 0) (num -1) ] ]
                (product [ coordinate 0, expt (velocity 0) (num 2) ])
    , test "Cannot solve 1D Lagrangian with no square-velocity term" <|
        \() ->
            assertNoAcceleration
                (velocity 0)
    , test "2D Lagrangian in gravity well" <|
        \() ->
            assertAcceleration
                [ num 0, num -0.5 ]
                (sum [ square (velocity 0), square (velocity 1), times (num -1) (coordinate 1) ])
    , test "2D Lagrangian with coordinate-dependent velocity" <|
        \() ->
            assertAcceleration
                [ product [ num -1, velocity 1, velocity 0, expt (coordinate 1) (num -1) ]
                , times (num 0.5) (square (velocity 0))
                ]
                (plus (times (square (velocity 0)) (coordinate 1)) (square (velocity 1)))
    , test "Cannot solve 2D Lagrangian with velocity cross-derivatives" <|
        \() ->
            assertNoAcceleration
                (times (square (velocity 0)) (square (velocity 1)))
    ]


assertAcceleration : List Expression -> Expression -> Expectation
assertAcceleration expected lagr =
    Expect.equal (Just expected) (solve lagr)


assertNoAcceleration : Expression -> Expectation
assertNoAcceleration lagr =
    Expect.equal Nothing (solve lagr)


accelerationTests : List Test
accelerationTests =
    let
        falling =
            (minus
                (times (num 0.5)
                    (plus (square (velocity 0)) (square (velocity 1)))
                )
                (times (num 10) (coordinate 1))
            )
                |> toAcceleration
                |> Maybe.withDefault (Mechanics.acceleration (always []))
    in
        [ test "gravitational potential" <|
            \() ->
                assertAboutEqual
                    (Mechanics.state 0.5 [ ( 3, 6 ), ( -1.25, -5 ) ])
                    (Mechanics.evolve falling 0.5 (Mechanics.state2 ( 0, 6 ) ( 0, 0 )))
        ]


assertAboutEqual : State -> State -> Expectation
assertAboutEqual a b =
    if Mechanics.aboutEqual 1.0e-10 a b then
        Expect.pass
    else
        Expect.equal a b
