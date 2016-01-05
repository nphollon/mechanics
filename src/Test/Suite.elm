module Test.Suite (all) where

import ElmTest exposing (..)

import Test.Expression
import Test.Lagrangian

all : Test
all =
  suite "Mechanics"
        [ Test.Lagrangian.all
        , Test.Expression.all
        ]
