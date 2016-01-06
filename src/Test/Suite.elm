module Test.Suite (all) where

import ElmTest exposing (..)

import Test.Expression
import Test.Lagrangian
import Test.Mechanics

all : Test
all =
  suite "Mechanics"
        [ Test.Mechanics.all
        , Test.Lagrangian.all
        , Test.Expression.all
        ]
