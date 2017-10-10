module Algorism.Addition.TypesTests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, intRange, tuple, string)
import String
import Algorism.Addition.Types exposing (Model, Column, CalculationState, calculateColumn, initializeColumnFor, initializeFor, numberOfDigits, parseNDigits, solve)
import Guarded.Input


inputIntModel : Guarded.Input.Model Int
inputIntModel =
    Guarded.Input.init


column : Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Column
column carry firstOperand secondOperand result =
    Column carry firstOperand secondOperand result inputIntModel inputIntModel


testSuite : Test
testSuite =
    describe "Algorithm.Addition.Types tests"
        [ describe "initializeColumnFor tests"
            [ fuzz2 int int "initializeColumnFor invoked with two somethings" <|
                \x y ->
                    column Nothing (Just x) (Just y) Nothing
                        |> Expect.equal (initializeColumnFor (Just x) (Just y))
            , fuzz int "initializeColumnFor invoked with one something" <|
                \x ->
                    column Nothing (Just x) Nothing Nothing
                        |> Expect.equal (initializeColumnFor (Just x) Nothing)
            , fuzz int "initializeColumnFor invoked with one other thing" <|
                \x ->
                    column Nothing Nothing (Just x) Nothing
                        |> Expect.equal (initializeColumnFor Nothing (Just x))
            , test "initializeColumnFor invoked with two nothings" <|
                \() ->
                    column Nothing Nothing Nothing Nothing
                        |> Expect.equal (initializeColumnFor Nothing Nothing)
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
        , describe "initializeFor tests"
            [ test "Negative first operand is not supported" <|
                \() ->
                    (Err "First operand is negative: -12")
                        |> Expect.equal (initializeFor -12 23)
            , test "Negative second operand is not supported" <|
                \() ->
                    (Err "Second operand is negative: -12")
                        |> Expect.equal (initializeFor 23 -12)
            , test "Two operands between 1 and 10" <|
                \() ->
                    Ok
                        (Model
                            [ column Nothing Nothing Nothing Nothing
                            , column Nothing (Just 7) (Just 2) Nothing
                            ]
                        )
                        |> Expect.equal (initializeFor 7 2)
            , fuzz (intRange 1 9) "One operand is less than 10, the other is zero" <|
                \operand ->
                    Ok
                        (Model
                            [ column Nothing Nothing Nothing Nothing
                            , column Nothing (Just operand) (Just 0) Nothing
                            ]
                        )
                        |> Expect.equal (initializeFor operand 0)
            , fuzz (intRange 1 9) "One operand is zero, other is less than 10" <|
                \operand ->
                    Ok
                        (Model
                            [ column Nothing Nothing Nothing Nothing
                            , column Nothing (Just 0) (Just operand) Nothing
                            ]
                        )
                        |> Expect.equal (initializeFor 0 operand)
            , fuzz (intRange 10 99) "One operand is less than 100, the other is zero " <|
                \operand ->
                    Ok
                        (Model
                            [ column Nothing Nothing Nothing Nothing
                            , column Nothing (Just <| operand // 10) Nothing Nothing
                            , column Nothing (Just <| rem operand 10) (Just 0) Nothing
                            ]
                        )
                        |> Expect.equal (initializeFor operand 0)
            , test "A 3-digit number and a 2-digit integer" <|
                \() ->
                    Ok
                        (Model
                            [ column Nothing Nothing Nothing Nothing
                            , column Nothing (Just 1) Nothing Nothing
                            , column Nothing (Just 2) (Just 7) Nothing
                            , column Nothing (Just 3) (Just 5) Nothing
                            ]
                        )
                        |> Expect.equal (initializeFor 123 75)
            , test "A 3-digit number and a 5-digit integer" <|
                \() ->
                    Ok
                        (Model
                            [ column Nothing Nothing Nothing Nothing
                            , column Nothing Nothing (Just 7) Nothing
                            , column Nothing Nothing (Just 5) Nothing
                            , column Nothing (Just 3) (Just 4) Nothing
                            , column Nothing (Just 2) (Just 3) Nothing
                            , column Nothing (Just 1) (Just 2) Nothing
                            ]
                        )
                        |> Expect.equal (initializeFor 321 75432)
            ]
        , describe "calculateColumn (internal function) tests"
            [ test "First column producing no carry" <|
                \() ->
                    { carry = 0
                    , columnsDone =
                        [ column Nothing (Just 4) (Just 3) (Just 7)
                        ]
                    }
                        |> Expect.equal
                            (calculateColumn
                                (column Nothing (Just 4) (Just 3) Nothing)
                                (CalculationState 0 [])
                            )
            , test "First column producing carry" <|
                \() ->
                    { carry = 1
                    , columnsDone =
                        [ column Nothing (Just 5) (Just 6) (Just 1)
                        ]
                    }
                        |> Expect.equal
                            (calculateColumn
                                (column Nothing (Just 5) (Just 6) Nothing)
                                (CalculationState 0 [])
                            )
            , test "Second column with no previous carry, not producing carry" <|
                \() ->
                    { carry = 0
                    , columnsDone =
                        [ column Nothing (Just 2) (Just 6) (Just 8)
                        , column Nothing (Just 3) (Just 4) (Just 7)
                        ]
                    }
                        |> Expect.equal
                            (calculateColumn
                                (column Nothing (Just 2) (Just 6) Nothing)
                                (CalculationState 0 [ (column Nothing (Just 3) (Just 4) (Just 7)) ])
                            )
            , test "Second column with no previous carry, producing carry" <|
                \() ->
                    { carry = 1
                    , columnsDone =
                        [ column Nothing (Just 4) (Just 6) (Just 0)
                        , column Nothing (Just 3) (Just 4) (Just 7)
                        ]
                    }
                        |> Expect.equal
                            (calculateColumn
                                (column Nothing (Just 4) (Just 6) Nothing)
                                (CalculationState 0 [ (column Nothing (Just 3) (Just 4) (Just 7)) ])
                            )
            , test "Second column with previous carry, producing no carry" <|
                \() ->
                    { carry = 0
                    , columnsDone =
                        [ column (Just 1) (Just 4) (Just 4) (Just 9)
                        , column Nothing (Just 7) (Just 4) (Just 1)
                        ]
                    }
                        |> Expect.equal
                            (calculateColumn
                                (column Nothing (Just 4) (Just 4) Nothing)
                                (CalculationState 1 [ (column Nothing (Just 7) (Just 4) (Just 1)) ])
                            )
            , test "Second column with previous carry, producing further carry" <|
                \() ->
                    { carry = 1
                    , columnsDone =
                        [ column (Just 1) (Just 5) (Just 4) (Just 0)
                        , column Nothing (Just 7) (Just 4) (Just 1)
                        ]
                    }
                        |> Expect.equal
                            (calculateColumn
                                (column Nothing (Just 5) (Just 4) Nothing)
                                (CalculationState 1 [ (column Nothing (Just 7) (Just 4) (Just 1)) ])
                            )
            ]
        , describe "Tests for solve function"
            [ test "0 + 0 = 0" <|
                \() ->
                    Model
                        [ column Nothing Nothing Nothing Nothing
                        , column Nothing (Just 0) (Just 0) (Just 0)
                        ]
                        |> Expect.equal
                            (solve
                                (Model
                                    [ column Nothing Nothing Nothing Nothing
                                    , column Nothing (Just 0) (Just 0) Nothing
                                    ]
                                )
                            )
            , test "1 + 0 = 0" <|
                \() ->
                    Model
                        [ column Nothing Nothing Nothing Nothing
                        , column Nothing (Just 1) (Just 0) (Just 1)
                        ]
                        |> Expect.equal
                            (solve
                                (Model
                                    [ column Nothing Nothing Nothing Nothing
                                    , column Nothing (Just 1) (Just 0) Nothing
                                    ]
                                )
                            )
            , test "1 + 8 = 9" <|
                \() ->
                    Model
                        [ column Nothing Nothing Nothing Nothing
                        , column Nothing (Just 1) (Just 8) (Just 9)
                        ]
                        |> Expect.equal
                            (solve
                                (Model
                                    [ column Nothing Nothing Nothing Nothing
                                    , column Nothing (Just 1) (Just 8) Nothing
                                    ]
                                )
                            )
            , test "2 + 8 = 10" <|
                \() ->
                    Model
                        [ column (Just 1) Nothing Nothing (Just 1)
                        , column Nothing (Just 2) (Just 8) (Just 0)
                        ]
                        |> Expect.equal
                            (solve
                                (Model
                                    [ column Nothing Nothing Nothing Nothing
                                    , column Nothing (Just 2) (Just 8) Nothing
                                    ]
                                )
                            )
            , test "13 + 24 = 37" <|
                \() ->
                    Model
                        [ column Nothing Nothing Nothing Nothing
                        , column Nothing (Just 1) (Just 2) (Just 3)
                        , column Nothing (Just 3) (Just 4) (Just 7)
                        ]
                        |> Expect.equal
                            (solve
                                (Model
                                    [ column Nothing Nothing Nothing Nothing
                                    , column Nothing (Just 1) (Just 2) Nothing
                                    , column Nothing (Just 3) (Just 4) Nothing
                                    ]
                                )
                            )
            , test "17 + 24 = 41" <|
                \() ->
                    Model
                        [ column Nothing Nothing Nothing Nothing
                        , column (Just 1) (Just 1) (Just 2) (Just 4)
                        , column Nothing (Just 7) (Just 4) (Just 1)
                        ]
                        |> Expect.equal
                            (solve
                                (Model
                                    [ column Nothing Nothing Nothing Nothing
                                    , column Nothing (Just 1) (Just 2) Nothing
                                    , column Nothing (Just 7) (Just 4) Nothing
                                    ]
                                )
                            )
            , test "63 + 36 = 99" <|
                \() ->
                    Model
                        [ column Nothing Nothing Nothing Nothing
                        , column Nothing (Just 6) (Just 3) (Just 9)
                        , column Nothing (Just 3) (Just 6) (Just 9)
                        ]
                        |> Expect.equal
                            (solve
                                (Model
                                    [ column Nothing Nothing Nothing Nothing
                                    , column Nothing (Just 6) (Just 3) Nothing
                                    , column Nothing (Just 3) (Just 6) Nothing
                                    ]
                                )
                            )
            , test "63 + 37 = 100" <|
                \() ->
                    Model
                        [ column (Just 1) Nothing Nothing (Just 1)
                        , column (Just 1) (Just 6) (Just 3) (Just 0)
                        , column Nothing (Just 3) (Just 7) (Just 0)
                        ]
                        |> Expect.equal
                            (solve
                                (Model
                                    [ column Nothing Nothing Nothing Nothing
                                    , column Nothing (Just 6) (Just 3) Nothing
                                    , column Nothing (Just 3) (Just 7) Nothing
                                    ]
                                )
                            )
            , test "173 + 37 = 210" <|
                \() ->
                    Model
                        [ column Nothing Nothing Nothing Nothing
                        , column (Just 1) (Just 1) Nothing (Just 2)
                        , column (Just 1) (Just 7) (Just 3) (Just 1)
                        , column Nothing (Just 3) (Just 7) (Just 0)
                        ]
                        |> Expect.equal
                            (solve
                                (Model
                                    [ column Nothing Nothing Nothing Nothing
                                    , column Nothing (Just 1) Nothing Nothing
                                    , column Nothing (Just 7) (Just 3) Nothing
                                    , column Nothing (Just 3) (Just 7) Nothing
                                    ]
                                )
                            )
            ]
        ]
