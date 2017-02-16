module Types exposing (Expression(..))


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
