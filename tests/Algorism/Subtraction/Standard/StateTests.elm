module Algorism.Subtraction.Standard.StateTests exposing (testSuite)

import Test exposing (..)
import Expect
import String
import Algorism.Subtraction.Standard.State exposing (update, updateIntColumn, updateBoolColumn)
import Algorism.Subtraction.Standard.Types exposing (..)
import Guarded.Input
import Guarded.Input.Parsers


defaultInputIntModel : Guarded.Input.Model Int
defaultInputIntModel =
    Guarded.Input.init


column : Maybe Int -> Bool -> Bool -> Maybe Int -> Maybe Int -> Maybe Int -> Column
column regrouppedFirstOperand borrowFromRegrouppedFirstOperand borrowFromFirstOperand firstOperand secondOperand result =
    { regrouppedFirstOperand = regrouppedFirstOperand
    , userRegrouppedFirstOperand = defaultInputIntModel
    , borrowFromRegrouppedFirstOperand = borrowFromRegrouppedFirstOperand
    , userBorrowFromRegrouppedFirstOperand = False
    , firstOperand = firstOperand
    , borrowFromFirstOperand = borrowFromFirstOperand
    , userBorrowFromFirstOperand = False
    , secondOperand = secondOperand
    , result = result
    , userResult = defaultInputIntModel
    }


someColumn : Column
someColumn =
    column Nothing False False (Just 1) (Just 2) Nothing


inputMsg5 : Guarded.Input.Msg Int
inputMsg5 =
    Guarded.Input.Parsers.decimalDigitParser "5"


inputIntModel5 : Guarded.Input.Model Int
inputIntModel5 =
    Guarded.Input.initFor 5


testSuite : Test
testSuite =
    describe "Algorism.Subtraction.Standard.State tests"
        [ describe "updateIntColumn tests"
            [ test "updating regroupped first operand (top) row with user input message" <|
                \() ->
                    { someColumn | userRegrouppedFirstOperand = inputIntModel5 }
                        |> Expect.equal
                            (updateIntColumn RegrouppedFirstOperand inputMsg5 someColumn |> Tuple.first)
            , test "updating result (bottom) row with user input message" <|
                \() ->
                    { someColumn | userResult = inputIntModel5 }
                        |> Expect.equal
                            (updateIntColumn Result inputMsg5 someColumn |> Tuple.first)
            ]
        , describe "updateBoolColumn tests"
            [ test "Toggling userBorrowFromRegrouppedFirstOperand True -> False with user input message" <|
                \() ->
                    let
                        originalColumn =
                            { someColumn | userBorrowFromRegrouppedFirstOperand = True }
                    in
                        { originalColumn | userBorrowFromRegrouppedFirstOperand = False }
                            |> Expect.equal
                                (updateBoolColumn BorrowFromRegrouppedFirstOperand originalColumn)
            , test "Toggling userBorrowFromFirstOperand False -> True with user input message" <|
                \() ->
                    { someColumn | userBorrowFromFirstOperand = True }
                        |> Expect.equal
                            (updateBoolColumn BorrowFromFirstOperand someColumn)
            ]
        ]
