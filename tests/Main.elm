port module Main exposing (..)

import Test exposing (describe)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Algorism.Common.UtilTests
import Algorism.Operands.StateTests
import Algorism.Addition.TypesTests
import Algorism.Addition.StateTests
import Algorism.Subtraction.Austrian.TypesTests
import Algorism.Subtraction.Austrian.StateTests
import Algorism.Subtraction.Standard.TypesTests
import Algorism.Subtraction.Standard.StateTests
import RouteTests


main : TestProgram
main =
    run emit <|
        describe "Arithmagic tests"
            [ Algorism.Common.UtilTests.testSuite
            , Algorism.Operands.StateTests.testSuite
            , Algorism.Addition.TypesTests.testSuite
            , Algorism.Addition.StateTests.testSuite
            , Algorism.Subtraction.Austrian.TypesTests.testSuite
            , Algorism.Subtraction.Austrian.StateTests.testSuite
            , Algorism.Subtraction.Standard.TypesTests.testSuite
            , Algorism.Subtraction.Standard.StateTests.testSuite
            , RouteTests.testSuite
            ]


port emit : ( String, Value ) -> Cmd msg
