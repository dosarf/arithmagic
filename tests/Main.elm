port module Main exposing (..)

import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Algorism.AdditionTests


main : TestProgram
main =
    run emit Algorism.AdditionTests.testSuite


port emit : ( String, Value ) -> Cmd msg
