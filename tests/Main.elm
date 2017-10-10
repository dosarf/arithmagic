port module Main exposing (..)

import Test exposing (describe)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Algorism.Operands.StateTests
import Algorism.Addition.TypesTests
import Algorism.Addition.StateTests
import Algorism.Subtraction.TypesTests
import Algorism.Subtraction.StateTests


main : TestProgram
main =
    run emit <|
        describe "Algorism tests"
            [ Algorism.Operands.StateTests.testSuite
            , Algorism.Addition.TypesTests.testSuite
            , Algorism.Addition.StateTests.testSuite
            , Algorism.Subtraction.TypesTests.testSuite
            , Algorism.Subtraction.StateTests.testSuite
            ]


port emit : ( String, Value ) -> Cmd msg
