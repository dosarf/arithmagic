module Algorism.Subtraction.Austrian.StateTests exposing (testSuite)

import Test exposing (..)
import Expect
import String
import Algorism.Subtraction.Austrian.State exposing (update, updateColumn)
import Algorism.Subtraction.Austrian.Types exposing (Column, EditableRow(..), Model, Msg(..), DigitInfo)
import Guarded.Input
import Guarded.Input.Parsers


defaultInputIntModel : Guarded.Input.Model Int
defaultInputIntModel =
    Guarded.Input.init


column : Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Column
column borrow firstOperand secondOperand result =
    Column borrow defaultInputIntModel firstOperand secondOperand result defaultInputIntModel


someColumn : Column
someColumn =
    column Nothing (Just 1) (Just 2) Nothing


inputMsg5 : Guarded.Input.Msg Int
inputMsg5 =
    Guarded.Input.Parsers.decimalDigitParser "5"


inputIntModel5 : Guarded.Input.Model Int
inputIntModel5 =
    Guarded.Input.initFor 5


someMsg : Msg
someMsg =
    DigitEdited (DigitInfo Borrow 1 inputMsg5)


someModel : Model
someModel =
    Model
        [ someColumn
        , someColumn
        ]


testSuite : Test
testSuite =
    describe "Algorism.Subtraction.Austrian.State tests"
        [ describe "updateColumn tests"
            [ test "updating borrow (top) row with user input message" <|
                \() ->
                    { someColumn | userBorrow = inputIntModel5 }
                        |> Expect.equal
                            (updateColumn Borrow inputMsg5 someColumn |> Tuple.first)
            , test "updating result (bottom) row with user input message" <|
                \() ->
                    { someColumn | userResult = inputIntModel5 }
                        |> Expect.equal
                            (updateColumn Result inputMsg5 someColumn |> Tuple.first)
            ]
        , describe "update tests"
            [ test "updating 1st colum, borrow (top) row with user input message" <|
                \() ->
                    Model
                        [ someColumn
                        , { someColumn | userBorrow = inputIntModel5 }
                        ]
                        |> Expect.equal (update someMsg someModel |> Tuple.first)
            ]
        ]
