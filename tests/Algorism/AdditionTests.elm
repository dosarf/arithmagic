module Algorism.AdditionTests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, intRange, tuple, string)
import String
import Algorism.Addition exposing (Model, Column, CalculationState, calculator, initColumn, initModel, initModelFor, numberOfDigits, parseNDigits, solve)


testSuite : Test
testSuite =
    describe "Algorithm.Addition tests"
        [ describe "Algorism.Addition.initModel tests"
            [ test "Algorism.Addition.initModel columnCount" <|
                \() ->
                    0
                        |> Expect.equal (initModel).columnCount
            , test "Algorism.Addition.initModel columns" <|
                \() ->
                    0
                        |> Expect.equal (List.length (initModel).columns)
            ]
        , describe "parseNDigits tests"
            [ test "Negative expected length yield empty list" <|
                \() ->
                    []
                        |> Expect.equal (parseNDigits -5 123)
            , test "Zero expected length yeilds empty list" <|
                \() ->
                    []
                        |> Expect.equal (parseNDigits 0 123)
            , test "With 0 value and 1 expected length you get a list of 0" <|
                \() ->
                    [ Just 0 ]
                        |> Expect.equal (parseNDigits 1 0)
            , test "With 0 value and 3 expected length you get a list of two nothings and a 0" <|
                \() ->
                    [ Nothing, Nothing, Just 0 ]
                        |> Expect.equal (parseNDigits 3 0)
            , test "Negative value yields empty list" <|
                \() ->
                    []
                        |> Expect.equal (parseNDigits 3 -123)
            , test "Parsing a single digit to one expected length yields that digit" <|
                \() ->
                    [ Just 5 ]
                        |> Expect.equal (parseNDigits 1 5)
            , test "Parsing a single digit to more expected length yields some padding and that digit" <|
                \() ->
                    [ Nothing, Nothing, Just 5 ]
                        |> Expect.equal (parseNDigits 3 5)
            , test "Parsing multiple digits to exact expected length yields those digits" <|
                \() ->
                    [ Just 1, Just 2, Just 3 ]
                        |> Expect.equal (parseNDigits 3 123)
            , test "Parsing multiple digits to more expected length yields some padding plus those digits" <|
                \() ->
                    [ Nothing, Nothing, Just 1, Just 2, Just 3 ]
                        |> Expect.equal (parseNDigits 5 123)
            ]
        , describe "numberOfDigits"
            [ test "digit number of 0 is 1" <|
                \() ->
                    1
                        |> Expect.equal (numberOfDigits 0)
            , test "digit number of 1 is 1" <|
                \() ->
                    1
                        |> Expect.equal (numberOfDigits 1)
            , test "digit number of 9 is 1" <|
                \() ->
                    1
                        |> Expect.equal (numberOfDigits 1)
            , test "digit number of 10 is 2" <|
                \() ->
                    2
                        |> Expect.equal (numberOfDigits 10)
            , fuzz (intRange 0 10000) "digit number of an integer = string length after conversion" <|
                \integer ->
                    String.length (toString integer)
                        |> Expect.equal (numberOfDigits integer)
            , test "digit number of -1 is 1" <|
                \() ->
                    1
                        |> Expect.equal (numberOfDigits -1)
            , test "digit number of -10 is 2" <|
                \() ->
                    2
                        |> Expect.equal (numberOfDigits -10)
            , fuzz (intRange -10000 -1) "digit number of an negative integer = string length after conversion of its absolute" <|
                \integer ->
                    String.length (toString -integer)
                        |> Expect.equal (numberOfDigits integer)
            , fuzz (intRange -1000 1000) "digit number of opposite numbers is equal" <|
                \integer ->
                    (numberOfDigits -integer)
                        |> Expect.equal (numberOfDigits integer)
            ]
        , describe "initColumn tests"
            [ fuzz2 int int "Initcolumn invoked with two somethings" <|
                \x y ->
                    (Column Nothing (Just x) (Just y) Nothing)
                        |> Expect.equal (initColumn (Just x) (Just y))
            , fuzz int "initColumn invoked with one something" <|
                \x ->
                    (Column Nothing (Just x) Nothing Nothing)
                        |> Expect.equal (initColumn (Just x) Nothing)
            , fuzz int "initColumn invoked with one other thing" <|
                \x ->
                    (Column Nothing Nothing (Just x) Nothing)
                        |> Expect.equal (initColumn Nothing (Just x))
            , test "initColumn invoked with two nothings" <|
                \() ->
                    (Column Nothing Nothing Nothing Nothing)
                        |> Expect.equal (initColumn Nothing Nothing)
            ]
        , describe "initModelFor tests"
            [ test "Negative first operand is not supported" <|
                \() ->
                    (Err "First operand is negative: -12")
                        |> Expect.equal (initModelFor -12 23)
            , test "Negative second operand is not supported" <|
                \() ->
                    (Err "Second operand is negative: -12")
                        |> Expect.equal (initModelFor 23 -12)
            , test "Two operands between 1 and 10" <|
                \() ->
                    (Ok
                        { columnCount = 2
                        , columns =
                            [ (Column Nothing Nothing Nothing Nothing)
                            , (Column Nothing (Just 7) (Just 2) Nothing)
                            ]
                        }
                    )
                        |> Expect.equal (initModelFor 7 2)
            , fuzz (intRange 1 9) "One operand is less than 10, the other is zero" <|
                \operand ->
                    (Ok
                        { columnCount = 2
                        , columns =
                            [ (Column Nothing Nothing Nothing Nothing)
                            , (Column Nothing (Just operand) (Just 0) Nothing)
                            ]
                        }
                    )
                        |> Expect.equal (initModelFor operand 0)
            , fuzz (intRange 1 9) "One operand is zero, other is less than 10" <|
                \operand ->
                    (Ok
                        { columnCount = 2
                        , columns =
                            [ (Column Nothing Nothing Nothing Nothing)
                            , (Column Nothing (Just 0) (Just operand) Nothing)
                            ]
                        }
                    )
                        |> Expect.equal (initModelFor 0 operand)
            , fuzz (intRange 10 99) "One operand is less than 100, the other is zero " <|
                \operand ->
                    (Ok
                        { columnCount = 3
                        , columns =
                            [ (Column Nothing Nothing Nothing Nothing)
                            , (Column Nothing (Just <| operand // 10) Nothing Nothing)
                            , (Column Nothing (Just <| rem operand 10) (Just 0) Nothing)
                            ]
                        }
                    )
                        |> Expect.equal (initModelFor operand 0)
            , test "A 3-digit number and a 2-digit integer" <|
                \() ->
                    (Ok
                        { columnCount = 4
                        , columns =
                            [ (Column Nothing Nothing Nothing Nothing)
                            , (Column Nothing (Just 1) Nothing Nothing)
                            , (Column Nothing (Just 2) (Just 7) Nothing)
                            , (Column Nothing (Just 3) (Just 5) Nothing)
                            ]
                        }
                    )
                        |> Expect.equal (initModelFor 123 75)
            , test "A 3-digit number and a 5-digit integer" <|
                \() ->
                    (Ok
                        { columnCount = 6
                        , columns =
                            [ (Column Nothing Nothing Nothing Nothing)
                            , (Column Nothing Nothing (Just 7) Nothing)
                            , (Column Nothing Nothing (Just 5) Nothing)
                            , (Column Nothing (Just 3) (Just 4) Nothing)
                            , (Column Nothing (Just 2) (Just 3) Nothing)
                            , (Column Nothing (Just 1) (Just 2) Nothing)
                            ]
                        }
                    )
                        |> Expect.equal (initModelFor 321 75432)
            ]
        , describe "calculator (internal function) tests"
            [ test "First column producing no carry" <|
                \() ->
                    { carry = 0
                    , columnsDone =
                        [ (Column Nothing (Just 4) (Just 3) (Just 7))
                        ]
                    }
                        |> Expect.equal
                            (calculator
                                (Column Nothing (Just 4) (Just 3) Nothing)
                                (CalculationState 0 [])
                            )
            , test "First column producing carry" <|
                \() ->
                    { carry = 1
                    , columnsDone =
                        [ (Column Nothing (Just 5) (Just 6) (Just 1))
                        ]
                    }
                        |> Expect.equal
                            (calculator
                                (Column Nothing (Just 5) (Just 6) Nothing)
                                (CalculationState 0 [])
                            )
            , test "Second column with no previous carry, not producing carry" <|
                \() ->
                    { carry = 0
                    , columnsDone =
                        [ (Column Nothing (Just 2) (Just 6) (Just 8))
                        , (Column Nothing (Just 3) (Just 4) (Just 7))
                        ]
                    }
                        |> Expect.equal
                            (calculator
                                (Column Nothing (Just 2) (Just 6) Nothing)
                                (CalculationState 0 [ (Column Nothing (Just 3) (Just 4) (Just 7)) ])
                            )
            , test "Second column with no previous carry, producing carry" <|
                \() ->
                    { carry = 1
                    , columnsDone =
                        [ (Column Nothing (Just 4) (Just 6) (Just 0))
                        , (Column Nothing (Just 3) (Just 4) (Just 7))
                        ]
                    }
                        |> Expect.equal
                            (calculator
                                (Column Nothing (Just 4) (Just 6) Nothing)
                                (CalculationState 0 [ (Column Nothing (Just 3) (Just 4) (Just 7)) ])
                            )
            , test "Second column with previous carry, producing no carry" <|
                \() ->
                    { carry = 0
                    , columnsDone =
                        [ (Column (Just 1) (Just 4) (Just 4) (Just 9))
                        , (Column Nothing (Just 7) (Just 4) (Just 1))
                        ]
                    }
                        |> Expect.equal
                            (calculator
                                (Column Nothing (Just 4) (Just 4) Nothing)
                                (CalculationState 1 [ (Column Nothing (Just 7) (Just 4) (Just 1)) ])
                            )
            , test "Second column with previous carry, producing further carry" <|
                \() ->
                    { carry = 1
                    , columnsDone =
                        [ (Column (Just 1) (Just 5) (Just 4) (Just 0))
                        , (Column Nothing (Just 7) (Just 4) (Just 1))
                        ]
                    }
                        |> Expect.equal
                            (calculator
                                (Column Nothing (Just 5) (Just 4) Nothing)
                                (CalculationState 1 [ (Column Nothing (Just 7) (Just 4) (Just 1)) ])
                            )
            ]
        , describe "Tests for solve function"
            [ test "0 + 0 = 0" <|
                \() ->
                    Model 2
                        [ (Column Nothing Nothing Nothing Nothing)
                        , (Column Nothing (Just 0) (Just 0) (Just 0))
                        ]
                        |> Expect.equal
                            (Model 2
                                [ (Column Nothing Nothing Nothing Nothing)
                                , (Column Nothing (Just 0) (Just 0) Nothing)
                                ]
                                |> solve
                            )
            , test "1 + 0 = 0" <|
                \() ->
                    Model 2
                        [ (Column Nothing Nothing Nothing Nothing)
                        , (Column Nothing (Just 1) (Just 0) (Just 1))
                        ]
                        |> Expect.equal
                            (Model 2
                                [ (Column Nothing Nothing Nothing Nothing)
                                , (Column Nothing (Just 1) (Just 0) Nothing)
                                ]
                                |> solve
                            )
            , test "1 + 8 = 9" <|
                \() ->
                    Model 2
                        [ (Column Nothing Nothing Nothing Nothing)
                        , (Column Nothing (Just 1) (Just 8) (Just 9))
                        ]
                        |> Expect.equal
                            (Model 2
                                [ (Column Nothing Nothing Nothing Nothing)
                                , (Column Nothing (Just 1) (Just 8) Nothing)
                                ]
                                |> solve
                            )
            , test "2 + 8 = 10" <|
                \() ->
                    Model 2
                        [ (Column (Just 1) Nothing Nothing (Just 1))
                        , (Column Nothing (Just 2) (Just 8) (Just 0))
                        ]
                        |> Expect.equal
                            (Model 2
                                [ (Column Nothing Nothing Nothing Nothing)
                                , (Column Nothing (Just 2) (Just 8) Nothing)
                                ]
                                |> solve
                            )
            , test "13 + 24 = 37" <|
                \() ->
                    Model 2
                        [ (Column Nothing Nothing Nothing Nothing)
                        , (Column Nothing (Just 1) (Just 2) (Just 3))
                        , (Column Nothing (Just 3) (Just 4) (Just 7))
                        ]
                        |> Expect.equal
                            (Model 2
                                [ (Column Nothing Nothing Nothing Nothing)
                                , (Column Nothing (Just 1) (Just 2) Nothing)
                                , (Column Nothing (Just 3) (Just 4) Nothing)
                                ]
                                |> solve
                            )
            , test "17 + 24 = 41" <|
                \() ->
                    Model 2
                        [ (Column Nothing Nothing Nothing Nothing)
                        , (Column (Just 1) (Just 1) (Just 2) (Just 4))
                        , (Column Nothing (Just 7) (Just 4) (Just 1))
                        ]
                        |> Expect.equal
                            (Model 2
                                [ (Column Nothing Nothing Nothing Nothing)
                                , (Column Nothing (Just 1) (Just 2) Nothing)
                                , (Column Nothing (Just 7) (Just 4) Nothing)
                                ]
                                |> solve
                            )
            , test "63 + 36 = 99" <|
                \() ->
                    Model 2
                        [ (Column Nothing Nothing Nothing Nothing)
                        , (Column Nothing (Just 6) (Just 3) (Just 9))
                        , (Column Nothing (Just 3) (Just 6) (Just 9))
                        ]
                        |> Expect.equal
                            (Model 2
                                [ (Column Nothing Nothing Nothing Nothing)
                                , (Column Nothing (Just 6) (Just 3) Nothing)
                                , (Column Nothing (Just 3) (Just 6) Nothing)
                                ]
                                |> solve
                            )
            , test "63 + 37 = 100" <|
                \() ->
                    Model 2
                        [ (Column (Just 1) Nothing Nothing (Just 1))
                        , (Column (Just 1) (Just 6) (Just 3) (Just 0))
                        , (Column Nothing (Just 3) (Just 7) (Just 0))
                        ]
                        |> Expect.equal
                            (Model 2
                                [ (Column Nothing Nothing Nothing Nothing)
                                , (Column Nothing (Just 6) (Just 3) Nothing)
                                , (Column Nothing (Just 3) (Just 7) Nothing)
                                ]
                                |> solve
                            )
            , test "173 + 37 = 210" <|
                \() ->
                    Model 2
                        [ (Column Nothing Nothing Nothing Nothing)
                        , (Column (Just 1) (Just 1) Nothing (Just 2))
                        , (Column (Just 1) (Just 7) (Just 3) (Just 1))
                        , (Column Nothing (Just 3) (Just 7) (Just 0))
                        ]
                        |> Expect.equal
                            (Model 2
                                [ (Column Nothing Nothing Nothing Nothing)
                                , (Column Nothing (Just 1) Nothing Nothing)
                                , (Column Nothing (Just 7) (Just 3) Nothing)
                                , (Column Nothing (Just 3) (Just 7) Nothing)
                                ]
                                |> solve
                            )
            ]
        ]
