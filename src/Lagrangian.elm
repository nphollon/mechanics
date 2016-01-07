module Lagrangian (num, time, coordinate, velocity, plus, minus, times, over, sum, product, square, expt, sine, cosine, ln, eval, Expression) where

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
    Sum [ a, b ]


minus : Expression -> Expression -> Expression
minus a b =
    a `plus` ((num -1) `times` b)


times : Expression -> Expression -> Expression
times a b =
    Prod [ a, b ]


over : Expression -> Expression -> Expression
over a b =
    a `times` (b `expt` (num -1))


square : Expression -> Expression
square base =
    base `expt` (num 2)


expt : Expression -> Expression -> Expression
expt base power =
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


sum : List Expression -> Expression
sum terms =
    Sum terms


product : List Expression -> Expression
product factors =
    Prod factors


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
