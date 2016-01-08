module Lagrangian (num, time, coordinate, velocity, plus, minus, times, over, sum, product, square, expt, sine, cosine, ln, eval, partial, Expression) where

import Mechanics as Mech exposing (State)


type Expression
    = Const Float
    | Time
    | Coord Int
    | Vel Int
    | Sum (List Expression)
    | Prod (List Expression)
    | Pow Expression Expression
    | Log Expression
    | Sin Expression
    | Cos Expression


num : Float -> Expression
num c =
    Const c


time : Expression
time =
    Time


coordinate : Int -> Expression
coordinate i =
    Coord i


velocity : Int -> Expression
velocity i =
    Vel i


plus : Expression -> Expression -> Expression
plus a b =
    sum [ a, b ]


minus : Expression -> Expression -> Expression
minus a b =
    sum [ a, (num -1) `times` b ]


sum : List Expression -> Expression
sum terms =
    let
        addTerm term ( const, symbols ) =
            case term of
                Const n ->
                    ( n + const, symbols )

                Sum ((Const n) :: subsymbols) ->
                    ( n + const, subsymbols ++ symbols )

                Sum subsymbols ->
                    ( const, subsymbols ++ symbols )

                otherwise ->
                    ( const, term :: symbols )

        reduced = List.foldr addTerm ( 0, [] ) terms
    in
        case reduced of
            ( c, [] ) ->
                num c

            ( 0, x :: [] ) ->
                x

            ( 0, xs ) ->
                Sum xs

            ( c, xs ) ->
                Sum ((num c) :: xs)


times : Expression -> Expression -> Expression
times a b =
    product [ a, b ]


over : Expression -> Expression -> Expression
over a b =
    product [ a, b `expt` (num -1) ]


product : List Expression -> Expression
product factors =
    let
        addFactor factor ( const, symbols ) =
            case factor of
                Const n ->
                    ( n * const, symbols )

                Prod ((Const n) :: subsymbols) ->
                    ( n * const, subsymbols ++ symbols )

                Prod subsymbols ->
                    ( const, subsymbols ++ symbols )

                otherwise ->
                    ( const, factor :: symbols )

        reduced = List.foldr addFactor ( 1, [] ) factors
    in
        case reduced of
            ( 0, _ ) ->
                num 0

            ( c, [] ) ->
                num c

            ( 1, x :: [] ) ->
                x

            ( 1, xs ) ->
                Prod xs

            ( c, xs ) ->
                Prod ((num c) :: xs)


square : Expression -> Expression
square base =
    base `expt` (num 2)


expt : Expression -> Expression -> Expression
expt base power =
    case power of
        Const 0 ->
            num 1

        Const 1 ->
            base

        otherwise ->
            Pow base power


sine : Expression -> Expression
sine x =
    Sin x


cosine : Expression -> Expression
cosine x =
    Cos x


ln : Expression -> Expression
ln x =
    Log x


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

        Prod factors ->
            List.map (flip eval state) factors |> List.product

        Pow base power ->
            (eval base state) ^ (eval power state)

        Log x ->
            logBase e (eval x state)

        Sin x ->
            sin (eval x state)

        Cos x ->
            cos (eval x state)


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

                Prod factors ->
                    let
                        productTerm u =
                            product ((recurse u) :: (List.filter ((/=) u) factors))
                    in
                        List.map productTerm factors |> sum

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
                    product [ num -1, sine u, recurse u ]

                Log u ->
                    (expt u (num -1)) `times` (recurse u)
