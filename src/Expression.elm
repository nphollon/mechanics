module Expression (num, time, coordinate, velocity, negative, plus, minus, times, over, sum, product, square, inverse, expt, sine, cosine, ln, dimension, getFloat, print, partial, eval, Expression) where

{-| Create symbolic mathematic expressions. For use with the `Lagrangian` module. The variable names are designed to correspond with the fields in `Mechanics.State`.

# Using expressions
@docs Expression, eval

# Expression primitives
@docs num, time, coordinate, velocity

# Expression arithmetic
@docs sum, plus, minus, product, times, over, negative, inverse, square, expt, sine, cosine, ln

# Inspecting expressions
@docs dimension, getFloat, print

# Taking derivatives
@docs partial
-}

import String
import Mechanics as Mech exposing (State)
import Types exposing (Expression(..))


{-| -}
type alias Expression =
    Types.Expression


{-| Encode a number as an expression.

    print (num 3.14) == "3.14"
-}
num : Float -> Expression
num c =
    Const c


{-| A variable representing time.

    print time == "t"
-}
time : Expression
time =
    Time


{-| A variable representing the coordinate with the given index.

    print (coordinate 0) == "x_0"
-}
coordinate : Int -> Expression
coordinate i =
    Coord i


{-| A variable representing the velocity with the given index.

    print (velocity 0) == "v_0"
-}
velocity : Int -> Expression
velocity i =
    Vel i


{-| Multiply an expression by -1.

    print (negative (num 2)) == "-2"
    print (negative time) == "(-1 t)"
-}
negative : Expression -> Expression
negative x =
    (num -1) `times` x


{-| Add two expressions.

    print ((num 3) `plus` (num 4)) == "7"
    print ((num 3) `plus` (velocity 2)) == "(3 + v_2)"
    print ((coordinate 1) `plus` (num 0)) == "x_1"
-}
plus : Expression -> Expression -> Expression
plus a b =
    sum [ a, b ]


{-| Subtract two expressions.

    print ((num 3) `minus` time) == "(3 + (-1 t))"
-}
minus : Expression -> Expression -> Expression
minus a b =
    sum [ a, negative b ]


{-| Add a list of expressions

    print (sum [ num -5, time, coordinate 0, num 8 ]) == "(-3 + t + x_0)"
-}
sum : List Expression -> Expression
sum terms =
    let
        findTerm query symbols =
            List.partition (\symbol -> fst symbol == query) symbols

        raiseCoeff term coeff symbols =
            case findTerm term symbols of
                ( [], s ) ->
                    ( term, coeff ) :: s

                ( r, s ) ->
                    ( term, coeff + List.sum (List.map snd r) ) :: s

        addTerm term ( const, symbols ) =
            case term of
                Const n ->
                    ( n + const, symbols )

                Sum subsymbols ->
                    List.foldr addTerm ( const, symbols ) subsymbols

                Prod coeff factors ->
                    ( const, raiseCoeff factors coeff symbols )

                otherwise ->
                    ( const, raiseCoeff [ term ] 1 symbols )

        ( const, symbols ) =
            List.foldr addTerm ( 0, [] ) terms

        reducedSymbols =
            List.map (\( factors, coeff ) -> product (num coeff :: factors)) symbols
    in
        case ( const, reducedSymbols ) of
            ( c, [] ) ->
                num c

            ( 0, x :: [] ) ->
                x

            ( 0, xs ) ->
                Sum xs

            ( c, xs ) ->
                Sum ((num c) :: xs)


{-| Multiply two expressions.

    print ((num 14) `times` (num 0.5)) == "7"
    print ((num 2) `times` time) == "(2 t)"
    print ((num 0) `times` time) == "0"
    print ((num 1) `times` time) == "t"
-}
times : Expression -> Expression -> Expression
times a b =
    product [ a, b ]


{-| Divide two expressions.

    print ((num 2) `over` time) == "(2 (t ^ -1))"
-}
over : Expression -> Expression -> Expression
over a b =
    product [ a, b `expt` (num -1) ]


{-| Multiply a list of factors.

    print (product [ num -5, time, coordinate 0, num 8 ]) == "(-40 t x_0)"
-}
product : List Expression -> Expression
product factors =
    let
        findFactor query symbols =
            List.partition (\symbol -> fst symbol == query) symbols

        raisePower base power symbols =
            case findFactor base symbols of
                ( [], s ) ->
                    ( base, power ) :: s

                ( r, s ) ->
                    ( base, sum (power :: (List.map snd r)) ) :: s

        addFactor factor ( const, symbols ) =
            case factor of
                Const n ->
                    ( n * const, symbols )

                Prod coeff subfactors ->
                    List.foldr addFactor ( const * coeff, symbols ) subfactors

                Pow base power ->
                    ( const, raisePower base power symbols )

                _ ->
                    ( const, raisePower factor (num 1) symbols )

        ( const, symbols ) =
            List.foldr addFactor ( 1, [] ) factors

        reducedSymbols =
            List.map (\( base, power ) -> expt base power) symbols
    in
        case ( const, reducedSymbols ) of
            ( 0, _ ) ->
                num 0

            ( c, [] ) ->
                num c

            ( 1, x :: [] ) ->
                x

            ( c, xs ) ->
                Prod c xs


{-| Raise an expression to the power of 2.

    print (square (velocity 0)) == "(v_0 ^ 2)"
-}
square : Expression -> Expression
square base =
    base `expt` (num 2)


{-| Raise an expression to the power of -1.

    print (inverse time) == "(t ^ -1)"
-}
inverse : Expression -> Expression
inverse x =
    x `expt` (num -1)


{-| Raise the first expression to the power of the second expression.

    print (expt (num 2) (num 3)) == "8"
    print (expt (num 2) time) == "(2 ^ t)"
    print (expt (expt (coordinate 0) time) (num 2)) == "(x_0 ^ (2 t))"
    print (expt time (num 0)) == "1"
    print (expt time (num 1)) == "t"
-}
expt : Expression -> Expression -> Expression
expt base power =
    case ( base, power ) of
        ( _, Const 0 ) ->
            num 1

        ( _, Const 1 ) ->
            base

        ( Const c, Const d ) ->
            num (c ^ d)

        ( Prod coeff factors, _ ) ->
            List.map (flip expt power) (num coeff :: factors) |> product

        ( Pow subbase subpower, _ ) ->
            expt subbase (subpower `times` power)

        ( _, _ ) ->
            Pow base power


{-| Take the sine of an expression.

    print (sine time) == "(sin t)"
-}
sine : Expression -> Expression
sine x =
    try sin x |> Maybe.withDefault (Sin x)


{-| Take the cosine of an expression.

    print (cosine time) == "(cos t)"
-}
cosine : Expression -> Expression
cosine x =
    try cos x |> Maybe.withDefault (Cos x)


{-| Take the natural logarithm of an expression.

    print (ln (num e)) == "1"
    print (ln (coordinate 1)) == "(ln x_1)"
-}
ln : Expression -> Expression
ln x =
    try (logBase e) x |> Maybe.withDefault (Log x)


try : (Float -> Float) -> Expression -> Maybe Expression
try f x =
    Maybe.map (f >> num) (getFloat x)


{-| Return the size of the coordinate/velocity vector used by the expression.
This is one plus the highest index used.

    dimension (num 8) == 0
    dimension (sine (time)) == 0
    dimension (((num 2) `times` time) `plus` (coordinate 0)) == 1
    dimension ((coordinate 0) `times` (coordinate 1)) == 2
    dimension (square (velocity 2)) == 3
-}
dimension : Expression -> Int
dimension expr =
    case expr of
        Const _ ->
            0

        Time ->
            0

        Coord i ->
            i + 1

        Vel i ->
            i + 1

        Sum terms ->
            List.map dimension terms |> List.maximum |> Maybe.withDefault 0

        Prod _ factors ->
            List.map dimension factors |> List.maximum |> Maybe.withDefault 0

        Pow x y ->
            max (dimension x) (dimension y)

        Log x ->
            dimension x

        Sin x ->
            dimension x

        Cos x ->
            dimension x


{-| If the expression equals a constant, return the constant. Otherwise, return `Nothing`.

    getFloat (num 3) == Just 3
    getFloat ((num 3) `times` (coordinate 0)) == Nothing
    getFloat ((num 0) `times` (coordinate 0)) == Just 0
-}
getFloat : Expression -> Maybe Float
getFloat x =
    case x of
        Const c ->
            Just c

        _ ->
            Nothing


{-| Convert the expression to a string. Examples are all over this page.
-}
print : Expression -> String
print expression =
    let
        enclose str =
            "(" ++ str ++ ")"

        joinEnclose joiner items =
            List.map print items
                |> String.join joiner
                |> enclose
    in
        case expression of
            Const c ->
                toString c

            Time ->
                "t"

            Coord i ->
                "x_" ++ (toString i)

            Vel i ->
                "v_" ++ (toString i)

            Sum terms ->
                joinEnclose " + " terms

            Prod coeff factors ->
                if coeff == 1 then
                    joinEnclose " " factors
                else
                    joinEnclose " " (num coeff :: factors)

            Pow x y ->
                (print x) ++ " ^ " ++ (print y) |> enclose

            Log x ->
                "ln " ++ (print x) |> enclose

            Sin x ->
                "sin " ++ (print x) |> enclose

            Cos x ->
                "cos " ++ (print x) |> enclose


{-| Take the partial derivative of the second expression with respect to the first.

    print (partial time (expt time (num 3))) == "(3 (t ^ 2))"
    print (partial (coordinate 0) (sine (coordinate 0))) == "(cos x_0)"
    print (partial time ((velocity 0) `times` (coordinate 0))) == "0"
-}
partial : Expression -> Expression -> Expression
partial variable function =
    let
        recurse = partial variable
    in
        if function == variable then
            num 1
        else
            case function of
                Const _ ->
                    num 0

                Time ->
                    num 0

                Coord _ ->
                    num 0

                Vel _ ->
                    num 0

                Prod coeff factors ->
                    let
                        productTerm u =
                            product ((recurse u) :: (List.filter ((/=) u) factors))
                    in
                        List.map productTerm factors |> sum |> times (num coeff)

                Sum terms ->
                    List.map recurse terms |> sum

                Pow base power ->
                    sum
                        [ product
                            [ power
                            , expt base (power `minus` (num 1))
                            , partial variable base
                            ]
                        , product
                            [ ln base
                            , expt base power
                            , partial variable power
                            ]
                        ]

                Sin u ->
                    (cosine u) `times` (recurse u)

                Cos u ->
                    (negative (sine u)) `times` (recurse u)

                Log u ->
                    (expt u (num -1)) `times` (recurse u)


{-| Evaluate an expression, replacing the variables in the expression with the
data in the state.

    data = Mechanics.state1 (3, 5)

    expression = (square (velocity 0)) `minus` (coordinate 0)

    eval expression data == 22

If the expression has a greater dimension than the state, the extra coordinate/velocity
components are assumed to be zero.

    Mechanics.state1 (3, 5)
        |> eval (velocity 1) == 0
-}
eval : Expression -> State -> Float
eval expr state =
    case expr of
        Const c ->
            c

        Time ->
            Mech.time state

        Coord i ->
            Mech.coordinate i state

        Vel i ->
            Mech.velocity i state

        Sum terms ->
            List.map (flip eval state) terms |> List.sum

        Prod coeff factors ->
            List.map (flip eval state) factors |> List.product |> (*) coeff

        Pow base power ->
            (eval base state) ^ (eval power state)

        Log x ->
            logBase e (eval x state)

        Sin x ->
            sin (eval x state)

        Cos x ->
            cos (eval x state)
