{- -
  This program will not work in the browser.
  To run on the command line, execute run-tests.sh
- -}

module Main where

import Console
import ElmTest
import Task
import LagrangianTest
import MechanicsTest


port runner : Signal (Task.Task a ())
port runner =
  ElmTest.suite "Mechanics"
          [ MechanicsTest.all
          , LagrangianTest.all
          ]
  |> ElmTest.consoleRunner
  |> Console.run
    
