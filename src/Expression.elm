module Expression (num, time, coordinate, velocity, negative, plus, minus, times, over, sum, product, square, inverse, expt, sine, cosine, ln, dimension, getFloat, partial, eval, Expression) where

import Mechanics as Mech exposing (State)


type Expression
    = Const Float
    | Time
    | Coord Int
    | Vel Int
    | Sum (List Expression)
    | Prod Float (List Expression)
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


negative : Expression -> Expression
negative x =
    (num -1) `times` x


plus : Expression -> Expression -> Expression
plus a b =
    sum [ a, b ]


minus : Expression -> Expression -> Expression
minus a b =
    sum [ a, negative b ]


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


times : Expression -> Expression -> Expression
times a b =
    product [ a, b ]


over : Expression -> Expression -> Expression
over a b =
    product [ a, b `expt` (num -1) ]


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


square : Expression -> Expression
square base =
    base `expt` (num 2)


inverse : Expression -> Expression
inverse x =
    x `expt` (num -1)


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


sine : Expression -> Expression
sine x =
    try sin x |> Maybe.withDefault (Sin x)


cosine : Expression -> Expression
cosine x =
    try cos x |> Maybe.withDefault (Cos x)


ln : Expression -> Expression
ln x =
    try (logBase e) x |> Maybe.withDefault (Log x)


try : (Float -> Float) -> Expression -> Maybe Expression
try f x =
    Maybe.map (f >> num) (getFloat x)


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


getFloat : Expression -> Maybe Float
getFloat x =
    case x of
        Const c ->
            Just c

        _ ->
            Nothing


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