module Expression where

type Expression =
  Parameter
    | LiteralFunction String
    | Constant Float
    | Sum (List Expression)
    | Product (List Expression)
    | Power Expression Expression
    | Log Expression
    | Sin Expression
    | Cos Expression

-- Useful extensions: Quotient, Difference, Pi, Exp
      
evalAtt : Float -> Expression -> Float
evalAtt t expr =
  case expr of
    
    Parameter ->
      t
      
    Constant c ->
      c
      
    Sum terms ->
      List.map (evalAtt t) terms
        |> List.sum

    Product factors ->
      List.map (evalAtt t) factors
        |> List.product

    Power base exponent ->
      (evalAtt t base) ^ (evalAtt t exponent)

    Log arg ->
      logBase e (evalAtt t arg)

    Sin arg ->
      sin (evalAtt t arg)

    Cos arg ->
      cos (evalAtt t arg)

    _ -> t
      
evalAt : Float -> Expression -> Expression
evalAt t expr =
  case expr of
    
    Parameter ->
      Constant t
      
    Sum terms ->
      List.map (evalAt t) terms
        |> sum

    Product factors ->
      List.map (evalAt t) factors
        |> product

    Power base exponent ->
      case (evalAt t base, evalAt t exponent) of
        (Constant x, Constant y) ->
          Constant (x ^ y)
        (a, Constant 1) ->
          a
        (a, b) ->
          Power a b

    Log arg ->
      case (evalAt t arg) of
        Constant x ->
          Constant (logBase e x)
        a ->
          Log a

    Sin arg ->
      case (evalAt t arg) of
        Constant x ->
          Constant (sin x)
        a ->
          Sin a

    Cos arg ->
      case (evalAt t arg) of
        Constant x ->
          Constant (cos x)
        a ->
          Cos a

    _ -> expr {-
-}

sum : List Expression -> Expression
sum terms =
  let
    partition term (total, abstracts) =
      case term of
        Constant c ->
          (total + c, abstracts)
        abstract ->
          (total, abstract :: abstracts)

    (finalTotal, allAbstracts) =
      List.foldr partition (0, []) terms

    reducedTerms =
      if finalTotal == 0 then
        allAbstracts
      else
        ((Constant finalTotal) :: allAbstracts)
  in
    case reducedTerms of
      [] ->
        Constant 0
                 
      [ singleton ] ->
        singleton
        
      list ->
        Sum list


product : List Expression -> Expression
product factors =
  let
    partition factor (total, abstracts) =
      case factor of
        Constant c ->
          (total * c, abstracts)
        abstract ->
          (total, abstract :: abstracts)

    (finalTotal, allAbstracts) =
      List.foldr partition (1, []) factors

  in
    if (List.isEmpty allAbstracts) then
      Constant finalTotal
    else
      Product ((Constant finalTotal) :: allAbstracts)

              
{-
replaceLiteral : String -> Expression -> Expression -> Expression

try : (Float -> Float) -> Expression -> Expression
-}
