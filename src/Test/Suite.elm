module Test.Suite (all) where

import ElmTest exposing (..)

import Test.Expression
import Test.Mechanics

all : Test
all =
  suite "Mechanics"
        [ Test.Mechanics.all
        , Test.Expression.all
        ]
