port module Main exposing (..)

import Test exposing (describe)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Algorism.Addition.AdditionTests
import Algorism.Operands.StateTests


main : TestProgram
main =
    run emit <|
        describe "Algorism tests"
            [ Algorism.Addition.AdditionTests.testSuite
            , Algorism.Operands.StateTests.testSuite
            ]


port emit : ( String, Value ) -> Cmd msg