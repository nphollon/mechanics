module LagrangianTest (..) where

import Array
import ElmTest exposing (..)
import Lagrangian exposing (..)
import Mechanics as Mech exposing (State)


all : Test
all =
    suite
        "Lagrangian mechanics"
        [ suite "Evaluating expressions" expressionTests
        , suite "Solving Lagrange equations" lagrangeTests
        ]


expressionTests : List Test
expressionTests =
    let
        c = -5

        is = { t = 2, x = 3, y = 5, vx = 7, vy = 11 }

        dummyState =
            Mech.state is.t [ ( is.x, is.vx ), ( is.y, is.vy ) ]
    in
        [ test "evaluating a constant"
            <| assertEval c (num c) dummyState
        , test "evaluating a time"
            <| assertEval is.t (time) dummyState
        , test "evaluating a coordinate"
            <| assertEval is.x (coordinate 0) dummyState
        , test "evaluating a velocity"
            <| assertEval is.vx (velocity 0) dummyState
        , test "adding two values"
            <| assertEval
                (is.vx + is.vy)
                ((velocity 0) `plus` (velocity 1))
                dummyState
        , test "adding multiple values"
            <| assertEval
                (is.vx + is.vy + is.y + is.x)
                (sum [ coordinate 0, coordinate 1, velocity 0, velocity 1 ])
                dummyState
        , test "subtracting two values"
            <| assertEval
                (is.vx - is.vy)
                ((velocity 0) `minus` (velocity 1))
                dummyState
        , test "multiplying two values"
            <| assertEval
                (is.vx * is.t)
                ((velocity 0) `times` time)
                dummyState
        , test "multiplying multiple values"
            <| assertEval
                (is.vx * is.x * is.t)
                (product [ velocity 0, coordinate 0, time ])
                dummyState
        , test "dividing two values"
            <| assertEval
                (is.x / is.t)
                ((coordinate 0) `over` time)
                dummyState
        , test "squaring a value"
            <| assertEval
                (is.x * is.x)
                (square (coordinate 0))
                dummyState
        , test "exponentiation"
            <| assertEval
                (is.x ^ c)
                (expt (coordinate 0) (num c))
                dummyState
        , test "natural logarithm"
            <| assertEval
                (logBase e is.x)
                (ln (coordinate 0))
                dummyState
        , test "sine"
            <| assertEval
                (sin is.t)
                (sine time)
                dummyState
        , test "cosine"
            <| assertEval
                (cos is.t)
                (cosine time)
                dummyState
        ]



{-
sum : List Expression -> Expression
product : List Expression -> Expression
-}


assertEval : Float -> Expression -> State -> Assertion
assertEval expected expr state =
    assertEqual expected (eval expr state)



{-
solveLagrangian : Expression -> List Expression
lagrangianToAcceleration : Expression -> Acceleration
-}


lagrangeTests : List Test
lagrangeTests =
    []
