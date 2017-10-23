module Algorism.Subtraction.Austrian.TypesTests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, intRange, tuple, string)
import String
import Algorism.Subtraction.Austrian.Types exposing (Model, Column, CalculationState, calculateColumn, solve)
import Guarded.Input


inputIntModel : Guarded.Input.Model Int
inputIntModel =
    Guarded.Input.init


column : Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Column
column borrow firstOperand secondOperand result =
    Column borrow firstOperand secondOperand result inputIntModel inputIntModel


testSuite : Test
testSuite =
    describe "Algorithm.Subtraction.Types tests"
        [ describe "calculateColumn (internal) function tests"
            [ fuzz (intRange 0 4) "no borrows of any kind" <|
                \x ->
                    let
                        firstOperand =
                            x + 5

                        secondOperand =
                            x
                    in
                        { borrow = 0
                        , columnsDone =
                            [ column Nothing (Just firstOperand) (Just secondOperand) (Just 5)
                            ]
                        }
                            |> Expect.equal
                                (calculateColumn
                                    (column Nothing (Just firstOperand) (Just secondOperand) Nothing)
                                    (CalculationState 0 [])
                                )
            , fuzz (intRange 0 3) "previous column borrowed from this one" <|
                \x ->
                    let
                        firstOperand =
                            x + 5

                        secondOperand =
                            x
                    in
                        { borrow = 0
                        , columnsDone =
                            [ column (Just 1) (Just firstOperand) (Just secondOperand) (Just 4)
                            ]
                        }
                            |> Expect.equal
                                (calculateColumn
                                    (column Nothing (Just firstOperand) (Just secondOperand) Nothing)
                                    (CalculationState 1 [])
                                )
            , fuzz (intRange 0 8) "borrowing from next column" <|
                \x ->
                    let
                        firstOperand =
                            x

                        secondOperand =
                            x + 1
                    in
                        { borrow = 1
                        , columnsDone =
                            [ column Nothing (Just firstOperand) (Just secondOperand) (Just 9)
                            ]
                        }
                            |> Expect.equal
                                (calculateColumn
                                    (column Nothing (Just firstOperand) (Just secondOperand) Nothing)
                                    (CalculationState 0 [])
                                )
            , fuzz (intRange 0 9) "borrowing from next column while previous column also borrowed from this one" <|
                \x ->
                    { borrow = 1
                    , columnsDone =
                        [ column (Just 1) (Just x) (Just x) (Just 9)
                        ]
                    }
                        |> Expect.equal
                            (calculateColumn
                                (column Nothing (Just x) (Just x) Nothing)
                                (CalculationState 1 [])
                            )
            , test "no second operand (digit) with no borrows" <|
                \() ->
                    { borrow = 0
                    , columnsDone =
                        [ column Nothing (Just 5) Nothing (Just 5)
                        ]
                    }
                        |> Expect.equal
                            (calculateColumn
                                (column Nothing (Just 5) Nothing Nothing)
                                (CalculationState 0 [])
                            )
            , test "no second operand (digit) with borrow from previous" <|
                \() ->
                    { borrow = 0
                    , columnsDone =
                        [ column (Just 1) (Just 5) Nothing (Just 4)
                        ]
                    }
                        |> Expect.equal
                            (calculateColumn
                                (column Nothing (Just 5) Nothing Nothing)
                                (CalculationState 1 [])
                            )
            , test "no second operand (digit) with borrow from previous borrowing from next" <|
                \() ->
                    { borrow = 1
                    , columnsDone =
                        [ column (Just 1) (Just 0) Nothing (Just 9)
                        ]
                    }
                        |> Expect.equal
                            (calculateColumn
                                (column Nothing (Just 0) Nothing Nothing)
                                (CalculationState 1 [])
                            )
            ]
        , describe "solve tests"
            [ test "subtracting from single digit" <|
                \() ->
                    Model
                        [ column Nothing (Just 5) (Just 3) (Just 2)
                        ]
                        |> Expect.equal
                            (solve
                                (Model
                                    [ column Nothing (Just 5) (Just 3) Nothing
                                    ]
                                )
                            )
            , test "subtracting single digit from multiple digits with borrow" <|
                \() ->
                    Model
                        [ column (Just 1) (Just 5) Nothing (Just 4)
                        , column Nothing (Just 2) (Just 3) (Just 9)
                        ]
                        |> Expect.equal
                            (solve
                                (Model
                                    [ column Nothing (Just 5) Nothing Nothing
                                    , column Nothing (Just 2) (Just 3) Nothing
                                    ]
                                )
                            )
            , test "subtracting multiple digits from multiple digits with borrow" <|
                \() ->
                    Model
                        [ column (Just 1) (Just 5) (Just 3) (Just 1)
                        , column Nothing (Just 2) (Just 3) (Just 9)
                        ]
                        |> Expect.equal
                            (solve
                                (Model
                                    [ column Nothing (Just 5) (Just 3) Nothing
                                    , column Nothing (Just 2) (Just 3) Nothing
                                    ]
                                )
                            )
            ]
        ]
