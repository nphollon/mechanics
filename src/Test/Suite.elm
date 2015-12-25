module Test.Suite (all) where

import ElmTest exposing (..)

import Expression exposing (..)

all : Test
all =
  suite "Evaluating expressions"
          [ test "Replace parameter with a float"
                   <| (evalAt 3 Parameter)
                        `is` 3
                   
          , test "Constant does not get replaced"
                   <| (evalAt 3 (Constant 9))
                        `is` 9

          , test "Adding a constant to a parameter"
                   <| (evalAt 3 (Sum [ Parameter, Constant 4 ]))
                        `is` 7

          , test "Adding a constant to a literal"
                   <| assertEqual
                        (Sum [ Constant 4, LiteralFunction "a" ])
                        (evalAt 3 (Sum [ Constant 4, LiteralFunction "a" ]))

          , test "t + a(t) + 5 = 7 + a(t) at t = 2"
                   <| assertEqual
                        (Sum [ Constant 7, LiteralFunction "a" ])
                        (evalAt 2 (Sum [ Parameter, LiteralFunction "a", Constant 5 ]))

          , test "a + b + 0 = a + b"
                   <| assertEqual
                        (Sum [ LiteralFunction "a", LiteralFunction "b" ])
                        (evalAt 0 (Sum [ Parameter, LiteralFunction "a", LiteralFunction "b" ]))

          , test "a + 0 = a"
                   <| assertEqual
                        (LiteralFunction "c")
                        (evalAt 1 (Sum [ Parameter, LiteralFunction "c", Constant -1 ]))

          , test "Multiplying a constant and a parameter"
                   <| (evalAt 3 (Product [ Parameter, Constant 4 ]))
                        `is` 12

          , test "Multiplying a constant and a literal"
                   <| assertEqual
                        (Product [ Constant 4, LiteralFunction "a" ])
                        (evalAt 3 (Product [ Constant 4, LiteralFunction "a" ]))

          , test "t * a(t) * 5 = 10 * a(t) at t = 2"
                   <| assertEqual
                        (Product [ Constant 10, LiteralFunction "a" ])
                        (evalAt 2 (Product [ Parameter, LiteralFunction "a", Constant 5 ]))

          , test "a * 0 = 0"
                   <| assertEqual
                        (Constant 0)
                        (evalAt 2 (Product [ LiteralFunction "a", Constant 0 ]))

          , test "a * 1 = a"
                   <| assertEqual
                        (LiteralFunction "a")
                        (evalAt 2 (Product [ LiteralFunction "a", Constant 1 ]))

          , test "Raising a parameter to a power"
                   <| (evalAt 2 (Power Parameter (Constant 4)))
                        `is` 16

          , test "a(t)^1 = a(t)"
                   <| assertEqual
                        (LiteralFunction "a")
                        (evalAt 3 (Power (LiteralFunction "a") (Constant 1)))

          , test "Taking a logarithm of a parameter"
                   <| (evalAt e (Log Parameter))
                        `is` 1

          , test "Sine of a parameter"
                   <| (evalAt (0.5 * pi) (Sin Parameter))
                        `is` 1

          , test "Cosine of a parameter"
                   <| (evalAt 2 (Cos Parameter))
                        `is` (cos 2)

          , test "Replacing a function literal with an expression"
                   <| assertEqual
                        (Cos Parameter)
                        (replaceLiteral "a" (Cos Parameter) (LiteralFunction "a"))

          , test "Replace all instances of a literal"
                   <| assertEqual
                        (Sum [ Sin Parameter, Cos Parameter ])
                        (replaceLiteral "a" Parameter
                         (Sum [ Sin (LiteralFunction "a"), Cos (LiteralFunction "a") ]))

          , test "Only replace literals with correct name"
                   <| assertEqual
                        (Power (LiteralFunction "base") (Constant 3))
                        (replaceLiteral "exp" (Constant 3)
                         (Power (LiteralFunction "base") (LiteralFunction "exp")))
          ]

{-
This is written so that if the assertion fails, the output will
tell you the values of a and b.
-}
assertAboutEqual : Float -> Float -> Assertion
assertAboutEqual a b =
  if (a - b)^2 < 1e-20 then
    assert True
  else
    assertEqual a b

                
is : Expression -> Float -> Assertion
is expr expected =
  case expr of
    Constant actual ->
      assertAboutEqual expected actual
                       
    _ ->
      assert False
