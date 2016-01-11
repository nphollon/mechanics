module Main (..) where

import Console
import ElmTest
import Task
import LagrangianTest
import MechanicsTest
import ExpressionTest


port runner : Signal (Task.Task a ())
port runner =
    ElmTest.suite
        "Mechanics"
        [ MechanicsTest.all
        , ExpressionTest.all
        , LagrangianTest.all
        ]
        |> ElmTest.consoleRunner
        |> Console.run
