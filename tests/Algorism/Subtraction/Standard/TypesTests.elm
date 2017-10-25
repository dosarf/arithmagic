module Algorism.Subtraction.Standard.TypesTests exposing (testSuite)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, intRange, tuple, string)
import String
import Algorism.Subtraction.Standard.Types exposing (Model, Column, CalculationState, calculateColumn, solve)
import Guarded.Input


inputIntModel : Guarded.Input.Model Int
inputIntModel =
    Guarded.Input.init


inputBoolModel : Bool
inputBoolModel =
    False


column : Maybe Int -> Bool -> Bool -> Maybe Int -> Maybe Int -> Maybe Int -> Column
column regrouppedFirstOperand borrowFromRegrouppedFirstOperand borrowFromFirstOperand firstOperand secondOperand result =
    { regrouppedFirstOperand = regrouppedFirstOperand
    , userRegrouppedFirstOperand = inputIntModel
    , borrowFromRegrouppedFirstOperand = borrowFromRegrouppedFirstOperand
    , userBorrowFromRegrouppedFirstOperand = inputBoolModel
    , firstOperand = firstOperand
    , borrowFromFirstOperand = borrowFromFirstOperand
    , userBorrowFromFirstOperand = inputBoolModel
    , secondOperand = secondOperand
    , result = result
    , userResult = inputIntModel
    }


testSuite : Test
testSuite =
    describe "Algorithm.Subtraction.Standard.Types tests"
        [ describe "calculateColumn (internal) function tests"
            [ test "no borrow from previous, no borrow from next" <|
                \() ->
                    { borrow = False
                    , columnsDone = [ column Nothing False False (Just 8) (Just 5) (Just 3) ]
                    }
                        |> Expect.equal
                            (calculateColumn
                                (column Nothing False False (Just 8) (Just 5) Nothing)
                                { borrow = False
                                , columnsDone = []
                                }
                            )
            , test "no borrow from previous, borrowing from next" <|
                \() ->
                    { borrow = True
                    , columnsDone = [ column (Just 14) False False (Just 4) (Just 5) (Just 9) ]
                    }
                        |> Expect.equal
                            (calculateColumn
                                (column Nothing False False (Just 4) (Just 5) Nothing)
                                { borrow = False
                                , columnsDone = []
                                }
                            )
            , test "simple borrow from previous, no borrowing from next" <|
                \() ->
                    { borrow = True
                    , columnsDone = [ column (Just 14) False False (Just 4) (Just 5) (Just 9) ]
                    }
                        |> Expect.equal
                            (calculateColumn
                                (column Nothing False False (Just 4) (Just 5) Nothing)
                                { borrow = False
                                , columnsDone = []
                                }
                            )
            , test "simple borrow from previous, borrowing from next" <|
                \() ->
                    { borrow = True
                    , columnsDone = [ column (Just 13) False True (Just 4) (Just 4) (Just 9) ]
                    }
                        |> Expect.equal
                            (calculateColumn
                                (column Nothing False False (Just 4) (Just 4) Nothing)
                                { borrow = True
                                , columnsDone = []
                                }
                            )
            , test "previous borrowing from zero (2nd operand is 0), borrowing from next" <|
                \() ->
                    { borrow = True
                    , columnsDone = [ column (Just 10) True False (Just 0) (Just 0) (Just 9) ]
                    }
                        |> Expect.equal
                            (calculateColumn
                                (column Nothing False False (Just 0) (Just 0) Nothing)
                                { borrow = True
                                , columnsDone = []
                                }
                            )
            , test "previous borrowing from zero (2nd operand is positive), borrowing from next" <|
                \() ->
                    { borrow = True
                    , columnsDone = [ column (Just 10) True False (Just 0) (Just 3) (Just 6) ]
                    }
                        |> Expect.equal
                            (calculateColumn
                                (column Nothing False False (Just 0) (Just 3) Nothing)
                                { borrow = True
                                , columnsDone = []
                                }
                            )
            ]
        , describe "solve tests"
            [ test "Subtracting single digit" <|
                \() ->
                    Model
                        [ column Nothing False False (Just 8) (Just 5) (Just 3)
                        ]
                        |> Expect.equal
                            (solve
                                (Model
                                    [ column Nothing False False (Just 8) (Just 5) Nothing
                                    ]
                                )
                            )
            , test "Subtracting with simple borrow" <|
                \() ->
                    Model
                        [ column Nothing False True (Just 8) (Just 5) (Just 2)
                        , column (Just 13) False False (Just 3) (Just 5) (Just 8)
                        ]
                        |> Expect.equal
                            (solve
                                (Model
                                    [ column Nothing False False (Just 8) (Just 5) Nothing
                                    , column Nothing False False (Just 3) (Just 5) Nothing
                                    ]
                                )
                            )
            , test "Subtracting with borrowing from zero(s)" <|
                \() ->
                    Model
                        [ column Nothing False True (Just 8) (Just 5) (Just 2)
                        , column (Just 10) True False (Just 0) (Just 5) (Just 4)
                        , column (Just 10) True False (Just 0) (Just 5) (Just 4)
                        , column (Just 13) False False (Just 3) (Just 5) (Just 8)
                        ]
                        |> Expect.equal
                            (solve
                                (Model
                                    [ column Nothing False False (Just 8) (Just 5) Nothing
                                    , column Nothing False False (Just 0) (Just 5) Nothing
                                    , column Nothing False False (Just 0) (Just 5) Nothing
                                    , column Nothing False False (Just 3) (Just 5) Nothing
                                    ]
                                )
                            )
            , test "Subtracting with consecutive borrows" <|
                \() ->
                    Model
                        [ column Nothing False True (Just 3) Nothing (Just 2)
                        , column (Just 14) False True (Just 5) (Just 6) (Just 8)
                        , column (Just 11) False False (Just 1) (Just 2) (Just 9)
                        ]
                        |> Expect.equal
                            (solve
                                (Model
                                    [ column Nothing False False (Just 3) Nothing Nothing
                                    , column Nothing False False (Just 5) (Just 6) Nothing
                                    , column Nothing False False (Just 1) (Just 2) Nothing
                                    ]
                                )
                            )
            ]
        ]
