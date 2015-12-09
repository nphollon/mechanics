module Expression where

type Expression =
  Parameter
    | Constant Float
    | Sum (List Expression)
    | Product (List Expression)
    | Power Expression Expression
    | Log Expression
    | Sin Expression
    | Cos Expression

-- Useful extensions: Quotient, Difference, Pi, Exp
      
evalAt : Float -> Expression -> Float
evalAt t expr =
  case expr of
    
    Parameter ->
      t
      
    Constant c ->
      c
      
    Sum terms ->
      List.map (evalAt t) terms
        |> List.sum

    Product factors ->
      List.map (evalAt t) factors
        |> List.product

    Power base exponent ->
      (evalAt t base) ^ (evalAt t exponent)

    Log arg ->
      logBase e (evalAt t arg)

    Sin arg ->
      sin (evalAt t arg)

    Cos arg ->
      cos (evalAt t arg)
