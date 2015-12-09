module Test.Suite (all) where

import ElmTest exposing (..)

import Expression exposing (..)

all : Test
all =
  suite "Evaluating expressions"
          [ test "Replace parameter with a float"
                   (assertEqual (evalAt 3 Parameter) 3)
                   
          , test "Constant does not get replaced"
                 (assertEqual (evalAt 3 (Constant 9)) 9)

          , test "Adding a constant to a parameter"
                   <| assertEqual
                        (evalAt 3 (Sum [ Parameter, Constant 4 ]))
                        7

          , test "Multiplying a constant and a parameter"
                   <| assertEqual
                        (evalAt 4 (Product [ Constant 2, Parameter ]))
                        8

          , test "Raising a parameter to a power"
                   <| assertEqual
                        (evalAt 2 (Power Parameter (Constant 4)))
                        16

          , test "Taking a logarithm of a parameter"
                   <| assertEqual
                        (evalAt e (Log Parameter))
                        1

          , test "Sine of a parameter"
                   <| assertAboutEqual
                        (evalAt (0.5 * pi) (Sin Parameter))
                        1

          , test "Cosine of a parameter"
                   <| assertAboutEqual
                        (evalAt 2 (Cos Parameter))
                        (cos 2)
          ]

assertAboutEqual : Float -> Float -> Assertion
assertAboutEqual a b =
  assert ((a - b)^2 < 1e-20)
