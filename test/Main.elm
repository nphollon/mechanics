{- -
  This program will not work in the browser.
  To run on the command line, execute run-tests.sh
- -}

module Main where

import Console
import ElmTest
import Task
import Test.Suite


port runner : Signal (Task.Task a ())
port runner =
  Console.run <| ElmTest.consoleRunner Test.Suite.all
    
