module Algorism.Addition.TypesTests exposing (testSuite)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, intRange, tuple, string)
import String
import Algorism.Addition.Types exposing (Model, Column, CalculationState, calculateColumn, initializeColumnFor, solve)
import Guarded.Input


inputIntModel : Guarded.Input.Model Int
inputIntModel =
    Guarded.Input.init


column : Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Column
column carry firstOperand secondOperand result =
    Column carry inputIntModel firstOperand secondOperand result inputIntModel


testSuite : Test
testSuite =
    describe "Algorism.Addition.Types tests"
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
