module ExpressionTest exposing (all)

import Expect exposing (Expectation)
import Test exposing (..)
import Expression exposing (..)
import Mechanics as Mech exposing (State)


all : Test
all =
    describe
        "Symbolic expressions"
        [ describe "Simplifying expressions" simplifyTests
        , describe "Examining expressions" examineTests
        , describe "Printing expressions" printTests
        , describe "Evaluating expressions" evalTests
        , describe "Partial differentiation" partialTests
        ]


simplifyTests : List Test
simplifyTests =
    [ test "multiplying 0 by x" <|
        \() ->
            assertNum
                0
                (product [ num 0, time ])
    , test "multiplying x by 0" <|
        \() ->
            assertNum
                0
                (product [ time, num 0 ])
    , test "multiplying 1 by x" <|
        \() ->
            Expect.equal
                time
                (product [ num 1, time ])
    , test "multiplying x by 1" <|
        \() ->
            Expect.equal
                time
                (product [ time, num 1 ])
    , test "adding 0 to x" <|
        \() ->
            Expect.equal
                time
                (sum [ num 0, time ])
    , test "summing to 0" <|
        \() ->
            assertNum
                0
                (sum [ num 1, num -1 ])
    , test "power of one" <|
        \() ->
            Expect.equal
                time
                (expt time (num 1))
    , test "power of zero" <|
        \() ->
            assertNum
                1
                (expt time (num 0))
    , test "nested multiplication" <|
        \() ->
            Expect.equal
                (product [ num 2, time, velocity 0, coordinate 1 ])
                (times (times (num 2) time)
                    (times (velocity 0) (coordinate 1))
                )
    , test "nested addition" <|
        \() ->
            Expect.equal
                (sum [ num 2, time, velocity 0, coordinate 1 ])
                (plus (plus (num 2) time)
                    (plus (velocity 0) (coordinate 1))
                )
    , test "constants raised to constant power" <|
        \() ->
            assertNum
                8
                (expt (num 2) (num 3))
    , test "log of a constant" <|
        \() ->
            assertNum
                1
                (ln (num e))
    , test "sin of constant" <|
        \() ->
            assertNum
                0
                (sine (num pi))
    , test "cos of constant" <|
        \() ->
            assertNum
                -1
                (cosine (num pi))
    , test "(ab)^c -> (a^c)(b^c)" <|
        \() ->
            Expect.equal
                (times (expt time (num 2)) (expt (coordinate 0) (num 2)))
                (expt (times time (coordinate 0)) (num 2))
    , test "(a^b)*(a^c)*d -> a^(b+c)*d" <|
        \() ->
            Expect.equal
                (times (expt time (num 5)) (coordinate 0))
                (times (times (expt time (num 3)) (expt time (num 2))) (coordinate 0))
    , test "n*a*b + m*a*b -> (m + n) * a * b if m and n are constants" <|
        \() ->
            Expect.equal
                (product [ num 3, time, coordinate 0 ])
                (plus (times time (coordinate 0)) (product [ num 2, time, coordinate 0 ]))
    , test "negation" <|
        \() ->
            Expect.equal
                (num -1)
                (negative (num 1))
    , test "inversion" <|
        \() ->
            Expect.equal
                (expt time (num -1))
                (inverse time)
    , test "(a^b)^c -> a^(b*c)" <|
        \() ->
            Expect.equal
                (expt time (num -2))
                (inverse (square time))
    ]


examineTests : List Test
examineTests =
    [ test "Expression with velocity 0 is 1-dimensional" <|
        \() ->
            Expect.equal
                1
                (dimension (velocity 0))
    , test "Expression with velocity 1 is 2-dimensional" <|
        \() ->
            Expect.equal
                2
                (dimension (plus (velocity 0) (velocity 1)))
    , test "Expression with coordinate 2 is 3-dimensional" <|
        \() ->
            Expect.equal
                3
                (dimension (times (velocity 0) (coordinate 2)))
    ]


printTests : List Test
printTests =
    [ test "constants are printed as floats" <|
        \() ->
            Expect.equal
                "1.5"
                (print (num 1.5))
    , test "time is printed as t" <|
        \() ->
            Expect.equal
                "t"
                (print time)
    , test "coordinate 1 is printed as x_1" <|
        \() ->
            Expect.equal
                "x_1"
                (print (coordinate 1))
    , test "velocity 1 is printed as v_1" <|
        \() ->
            Expect.equal
                "v_1"
                (print (velocity 1))
    , test "sine is printed in parens" <|
        \() ->
            Expect.equal
                "(sin t)"
                (print (sine time))
    , test "cosine is printed in parens" <|
        \() ->
            Expect.equal
                "(cos t)"
                (print (cosine time))
    , test "natural log is printed in parens" <|
        \() ->
            Expect.equal
                "(ln t)"
                (print (ln time))
    , test "exponentiation is printed in parens with a caret" <|
        \() ->
            Expect.equal
                "(x_0 ^ t)"
                (print (expt (coordinate 0) time))
    , test "sums are printed in parens with plusses interspersed" <|
        \() ->
            Expect.equal
                "(3 + x_0 + x_1)"
                (print (sum [ num 3, coordinate 0, coordinate 1 ]))
    , test "products are printed in parens with factors separated by spaces" <|
        \() ->
            Expect.equal
                "(3 x_0 x_1)"
                (print (product [ num 3, coordinate 0, coordinate 1 ]))
    , test "unit coefficient is not printed" <|
        \() ->
            Expect.equal
                "(t x_0)"
                (print (product [ num 1, time, coordinate 0 ]))
    ]


evalTests : List Test
evalTests =
    let
        c =
            -5

        is =
            { t = 2, x = 3, y = 5, vx = 7, vy = 11 }

        dummyState =
            Mech.state is.t [ ( is.x, is.vx ), ( is.y, is.vy ) ]
    in
        [ test "evaluating a constant" <|
            \() -> assertEval c (num c) dummyState
        , test "evaluating a time" <|
            \() -> assertEval is.t (time) dummyState
        , test "evaluating a coordinate" <|
            \() -> assertEval is.x (coordinate 0) dummyState
        , test "evaluating a velocity" <|
            \() -> assertEval is.vx (velocity 0) dummyState
        , test "adding two values" <|
            \() ->
                assertEval
                    (is.vx + is.vy)
                    (plus (velocity 0) (velocity 1))
                    dummyState
        , test "adding multiple values" <|
            \() ->
                assertEval
                    (is.vx + is.vy + is.y + is.x)
                    (sum [ coordinate 0, coordinate 1, velocity 0, velocity 1 ])
                    dummyState
        , test "subtracting two values" <|
            \() ->
                assertEval
                    (is.vx - is.vy)
                    (minus (velocity 0) (velocity 1))
                    dummyState
        , test "multiplying two values" <|
            \() ->
                assertEval
                    (is.vx * is.t)
                    (times (velocity 0) time)
                    dummyState
        , test "multiplying multiple values" <|
            \() ->
                assertEval
                    (is.vx * is.x * is.t)
                    (product [ velocity 0, coordinate 0, time ])
                    dummyState
        , test "dividing two values" <|
            \() ->
                assertEval
                    (is.x / is.t)
                    (over (coordinate 0) time)
                    dummyState
        , test "squaring a value" <|
            \() ->
                assertEval
                    (is.x * is.x)
                    (square (coordinate 0))
                    dummyState
        , test "exponentiation" <|
            \() ->
                assertEval
                    (is.x ^ c)
                    (expt (coordinate 0) (num c))
                    dummyState
        , test "natural logarithm" <|
            \() ->
                assertEval
                    (logBase e is.x)
                    (ln (coordinate 0))
                    dummyState
        , test "sine" <|
            \() ->
                assertEval
                    (sin is.t)
                    (sine time)
                    dummyState
        , test "cosine" <|
            \() ->
                assertEval
                    (cos is.t)
                    (cosine time)
                    dummyState
        ]


partialTests : List Test
partialTests =
    [ test "derivative of constant is zero" <|
        \() ->
            assertNum
                0
                (partial time (num 1))
    , test "dx/dx = 1" <|
        \() ->
            assertNum
                1
                (partial time time)
    , test "d(sin x)/dx = cos x" <|
        \() ->
            Expect.equal
                (cosine time)
                (partial time (sine time))
    , test "d(ln x)/dx = 1/x" <|
        \() ->
            Expect.equal
                (expt time (num -1))
                (partial time (ln time))
    , test "chain rule: d(sin (ln x))/dx" <|
        \() ->
            Expect.equal
                (times (cosine (ln time)) (expt time (num -1)))
                (partial time (sine (ln time)))
    , test "chain rule: d(ln (sin x))/dx" <|
        \() ->
            Expect.equal
                (times (expt (sine time) (num -1)) (cosine time))
                (partial time (ln (sine time)))
    , test "d(cos x)/dx = -sin x" <|
        \() ->
            Expect.equal
                (product [ num -1, sine (ln time), expt time (num -1) ])
                (partial time (cosine (ln time)))
    , test "d(x^3)/dx = 3 * x^2" <|
        \() ->
            Expect.equal
                (product [ num 3, expt time (num 2) ])
                (partial time (expt time (num 3)))
    , test "d(2^x)/dx = (ln 2) * 2^x" <|
        \() ->
            Expect.equal
                (times (ln (num 2)) (expt (num 2) time))
                (partial time (expt (num 2) time))
    , test "d(f + g)/dx = df/dx + dg/dx" <|
        \() ->
            Expect.equal
                (plus (times (num 2) time) (num 1))
                (partial time (plus (square time) time))
    , test "d(f * g)/dx = g * df/dx + f * dg/dx" <|
        \() ->
            Expect.equal
                (sum
                    [ product [ ln (num 2), expt (num 2) time, expt time (num 2) ]
                    , product [ num 2, time, expt (num 2) time ]
                    ]
                )
                (partial time (times (expt (num 2) time) (expt time (num 2))))
    , test "partial wrt time ignores other variables" <|
        \() ->
            Expect.equal
                (times (coordinate 0) (velocity 0))
                (partial time (product [ time, coordinate 0, velocity 0 ]))
    , test "partial wrt coordinate not equal to partial wrt time" <|
        \() ->
            Expect.equal
                time
                (partial (coordinate 0) (times time (coordinate 0)))
    ]


assertEval : Float -> Expression -> State -> Expectation
assertEval expected expr state =
    Expect.equal expected (eval expr state)


assertNum : Float -> Expression -> Expectation
assertNum expected expr =
    let
        fail =
            Expect.equal (num expected) expr
    in
        case getFloat expr of
            Just actual ->
                if (actual - expected) ^ 2 < 1.0e-20 then
                    Expect.pass
                else
                    fail

            Nothing ->
                fail
