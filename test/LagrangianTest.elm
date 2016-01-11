module LagrangianTest (..) where

import Array
import ElmTest exposing (..)
import Lagrangian exposing (..)
import Mechanics as Mech exposing (State)


all : Test
all =
    suite
        "Lagrangian mechanics"
        [ suite "Simplifying expressions" simplifyTests
        , suite "Evaluating expressions" expressionTests
        , suite "Partial differentiation" partialTests
        , suite "Solving Lagrange equations" lagrangeTests
        ]


simplifyTests : List Test
simplifyTests =
    [ test "multiplying 0 by x"
        <| assertNum
            0
            (product [ num 0, time ])
    , test "multiplying x by 0"
        <| assertNum
            0
            (product [ time, num 0 ])
    , test "multiplying 1 by x"
        <| assertEqual
            time
            (product [ num 1, time ])
    , test "multiplying x by 1"
        <| assertEqual
            time
            (product [ time, num 1 ])
    , test "adding 0 to x"
        <| assertEqual
            time
            (sum [ num 0, time ])
    , test "summing to 0"
        <| assertNum
            0
            (sum [ num 1, num -1 ])
    , test "power of one"
        <| assertEqual
            time
            (expt time (num 1))
    , test "power of zero"
        <| assertNum
            1
            (expt time (num 0))
    , test "nested multiplication"
        <| assertEqual
            (product [ num 2, time, velocity 0, coordinate 1 ])
            (num 2 `times` time `times` (velocity 0 `times` coordinate 1))
    , test "nested addition"
        <| assertEqual
            (sum [ num 2, time, velocity 0, coordinate 1 ])
            (num 2 `plus` time `plus` (velocity 0 `plus` coordinate 1))
    , test "constants raised to constant power"
        <| assertNum
            8
            (expt (num 2) (num 3))
    , test "log of a constant"
        <| assertNum
            1
            (ln (num e))
    , test "sin of constant"
        <| assertNum
            0
            (sine (num pi))
    , test "cos of constant"
        <| assertNum
            -1
            (cosine (num pi))
    , test "(ab)^c -> (a^c)(b^c)"
        <| assertEqual
            ((expt time (num 2)) `times` (expt (coordinate 0) (num 2)))
            (expt (time `times` (coordinate 0)) (num 2))
    , test "(a^b)*(a^c)*d -> a^(b+c)*d"
        <| assertEqual
            ((expt time (num 5)) `times` (coordinate 0))
            ((expt time (num 3)) `times` (expt time (num 2)) `times` (coordinate 0))
    , test "n*a*b + m*a*b -> (m + n) * a * b if m and n are constants"
        <| assertEqual
            (product [ num 3, time, coordinate 0 ])
            ((time `times` (coordinate 0)) `plus` (product [ num 2, time, coordinate 0 ]))
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


partialTests : List Test
partialTests =
    [ test "derivative of constant is zero"
        <| assertNum
            0
            (partial time (num 1))
    , test "dx/dx = 1"
        <| assertNum
            1
            (partial time time)
    , test "d(sin x)/dx = cos x"
        <| assertEqual
            (cosine time)
            (partial time (sine time))
    , test "d(ln x)/dx = 1/x"
        <| assertEqual
            (expt time (num -1))
            (partial time (ln time))
    , test "chain rule: d(sin (ln x))/dx"
        <| assertEqual
            ((cosine (ln time)) `times` (expt time (num -1)))
            (partial time (sine (ln time)))
    , test "chain rule: d(ln (sin x))/dx"
        <| assertEqual
            ((expt (sine time) (num -1)) `times` (cosine time))
            (partial time (ln (sine time)))
    , test "d(cos x)/dx = -sin x"
        <| assertEqual
            (product [ num -1, sine (ln time), expt time (num -1) ])
            (partial time (cosine (ln time)))
    , test "d(x^3)/dx = 3 * x^2"
        <| assertEqual
            (product [ num 3, expt time (num 2) ])
            (partial time (expt time (num 3)))
    , test "d(2^x)/dx = (ln 2) * 2^x"
        <| assertEqual
            ((ln (num 2)) `times` (expt (num 2) time))
            (partial time (expt (num 2) time))
    , test "d(f + g)/dx = df/dx + dg/dx"
        <| assertEqual
            (((num 2) `times` time) `plus` (num 1))
            (partial time ((square time) `plus` time))
    , test "d(f * g)/dx = g * df/dx + f * dg/dx"
        <| assertEqual
            (sum
                [ product [ ln (num 2), expt (num 2) time, expt time (num 2) ]
                , product [ num 2, time, expt (num 2) time ]
                ]
            )
            (partial time ((expt (num 2) time) `times` (expt time (num 2))))
    , test "partial wrt time ignores other variables"
        <| assertEqual
            (coordinate 0 `times` velocity 0)
            (partial time (product [ time, coordinate 0, velocity 0 ]))
    , test "partial wrt coordinate not equal to partial wrt time"
        <| assertEqual
            time
            (partial (coordinate 0) (time `times` (coordinate 0)))
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
    ]


assertEval : Float -> Expression -> State -> Assertion
assertEval expected expr state =
    assertEqual expected (eval expr state)


assertNum : Float -> Expression -> Assertion
assertNum expected expr =
    let
        fail =
            assertEqual (num expected) expr
    in
        case getFloat expr of
            Just actual ->
                if (actual - expected) ^ 2 < 1.0e-20 then
                    assert True
                else
                    fail

            Nothing ->
                fail


assertAcceleration : List Expression -> Expression -> Assertion
assertAcceleration expected lagr =
    assertEqual (Just expected) (solveLagrangian lagr)


assertNoAcceleration : Expression -> Assertion
assertNoAcceleration lagr =
    assertEqual Nothing (solveLagrangian lagr)
