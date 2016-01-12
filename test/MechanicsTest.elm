module MechanicsTest (..) where

import Array
import ElmTest exposing (..)
import Mechanics as Mech


all : Test
all =
    suite
        "Dynamics"
        [ suite "Building states" stateTests
        , suite "Evolving states" accelerationTests
        ]


stateTests : List Test
stateTests =
    [ test "state1 is sugar for 1-dimensional state"
        <| assertAboutEqual
            (Mech.state1 ( 1, 2 ))
            (Mech.state 0 [ ( 1, 2 ) ])
    , test "states are unequal if their coordinates differ"
        <| assertNotEqual
            (Mech.state1 ( 1, 2 ))
            (Mech.state 0 [ ( 2, 2 ) ])
    , test "states are unequal if their times differ"
        <| assertNotEqual
            (Mech.state1 ( 1, 2 ))
            (Mech.state 0.5 [ ( 1, 2 ) ])
    , test "state2 is sugar for 2-dimensional state"
        <| assertAboutEqual
            (Mech.state2 ( 4, 2 ) ( 3, 1 ))
            (Mech.state 0 [ ( 4, 2 ), ( 3, 1 ) ])
    , test "state3 is sugar for 3-dimensional state"
        <| assertAboutEqual
            (Mech.state3 ( 8, 9 ) ( 4, 2 ) ( 3, 1 ))
            (Mech.state 0 [ ( 8, 9 ), ( 4, 2 ), ( 3, 1 ) ])
    , test "dimension returns number of coordinates"
        <| assertEqual
            2
            (Mech.dimension (Mech.state2 ( 0, 0 ) ( 0, 0 )))
    , test "time returns time"
        <| assertEqual
            1.5
            (Mech.time (Mech.state 1.5 []))
    , test "coordinate returns coordinate with given index"
        <| assertEqual
            5
            (Mech.coordinate 1 (Mech.state2 ( 0, 0 ) ( 5, 0 )))
    , test "coordinate returns 0 if index is out of range"
        <| assertEqual
            0
            (Mech.coordinate 2 (Mech.state2 ( 0, 0 ) ( 5, 0 )))
    , test "velocity returns velocity with given index"
        <| assertEqual
            7
            (Mech.velocity 1 (Mech.state2 ( 0, 0 ) ( 0, 7 )))
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
            <| assertAboutEqual
                (Mech.state 1.0 [ ( 1, 0 ) ])
                (Mech.evolve inert 1.0 (Mech.state1 ( 1, 0 )))
        , test "position changes if velocity is non-zero"
            <| assertAboutEqual
                (Mech.state 1.0 [ ( 2, 1 ) ])
                (Mech.evolve inert 1.0 (Mech.state1 ( 1, 1 )))
        , test "dx = dt * v"
            <| assertAboutEqual
                (Mech.state 0.5 [ ( 1.5, 1 ) ])
                (Mech.evolve inert 0.5 (Mech.state1 ( 1, 1 )))
        , test "dv = dt * a"
            <| assertAboutEqual
                (Mech.state 0.5 [ ( -0.25, -1 ) ])
                (Mech.evolve falling 0.5 (Mech.state1 ( 0, 0 )))
        ]


assertAboutEqual : Mech.State -> Mech.State -> Assertion
assertAboutEqual a b =
    if Mech.aboutEqual 1.0e-10 a b then
        assert True
    else
        assertEqual a b
