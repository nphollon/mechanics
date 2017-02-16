port module Main exposing (..)

import Test
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import ExpressionTest
import LagrangianTest
import MechanicsTest


main : TestProgram
main =
    run emit <|
        Test.describe "All Tests"
            [ MechanicsTest.all
            , ExpressionTest.all
            , LagrangianTest.all
            ]


port emit : ( String, Value ) -> Cmd msg
