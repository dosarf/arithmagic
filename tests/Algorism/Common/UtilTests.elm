module Algorism.Common.UtilTests exposing (testSuite)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, intRange, tuple, string)
import String
import Algorism.Common.Operator exposing (Operator(..))
import Algorism.Common.Util exposing (..)


type alias Column =
    { firstOperand : Maybe Int
    , secondOperand : Maybe Int
    }


type alias Model =
    { columns : List Column
    }


testSuite : Test
testSuite =
    describe "Algorism.Common.Util tests"
        [ describe "numberOfDigits"
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
            , test "Parsing with too few expected digits truncates the highest digits" <|
                \() ->
                    [ Just 3, Just 4, Just 5 ]
                        |> Expect.equal (parseNDigits 3 12345)
            ]
        , describe "initializeForModel tests"
            [ test "Negative first operand is not supported for addition" <|
                \() ->
                    (Err "First operand is negative: -12")
                        |> Expect.equal (initializeForModel Addition Model Column -12 23)
            , test "Negative first operand is not supported for subtraction" <|
                \() ->
                    (Err "First operand is negative: -12")
                        |> Expect.equal (initializeForModel Subtraction Model Column -12 23)
            , test "Negative second operand is not supported for addition" <|
                \() ->
                    (Err "Second operand is negative: -12")
                        |> Expect.equal (initializeForModel Addition Model Column 23 -12)
            , test "Negative second operand is not supported for subtraction" <|
                \() ->
                    (Err "Second operand is negative: -12")
                        |> Expect.equal (initializeForModel Addition Model Column 23 -12)
            , test "First operand does not have to be greater than second one for addition" <|
                \() ->
                    Ok
                        (Model
                            [ Column Nothing Nothing
                            , Column (Just 1) (Just 2)
                            , Column (Just 2) (Just 3)
                            ]
                        )
                        |> Expect.equal (initializeForModel Addition Model Column 12 23)
            , test "First operand does must not be smaller than second one for subtraction" <|
                \() ->
                    (Err "First operand < second operand (12<23)")
                        |> Expect.equal (initializeForModel Subtraction Model Column 12 23)
            , test "First operand can be equal to the second one for subtraction" <|
                \() ->
                    Ok
                        (Model
                            [ Column (Just 4) (Just 4)
                            , Column (Just 2) (Just 2)
                            ]
                        )
                        |> Expect.equal (initializeForModel Subtraction Model Column 42 42)
            , test "Two operands between 1 and 10 added" <|
                \() ->
                    Ok
                        (Model
                            [ Column Nothing Nothing
                            , Column (Just 7) (Just 2)
                            ]
                        )
                        |> Expect.equal (initializeForModel Addition Model Column 7 2)
            , fuzz (intRange 1 9) "One operand is less than 10, the other is zero (addition)" <|
                \operand ->
                    Ok
                        (Model
                            [ Column Nothing Nothing
                            , Column (Just operand) (Just 0)
                            ]
                        )
                        |> Expect.equal (initializeForModel Addition Model Column operand 0)
            , fuzz (intRange 1 9) "One operand is less than 10, the other is zero (subtraction)" <|
                \operand ->
                    Ok
                        (Model
                            [ Column (Just operand) (Just 0)
                            ]
                        )
                        |> Expect.equal (initializeForModel Subtraction Model Column operand 0)
            , fuzz (intRange 10 99) "One operand is less than 100, the other is zero (addition)" <|
                \operand ->
                    Ok
                        (Model
                            [ Column Nothing Nothing
                            , Column (Just <| operand // 10) Nothing
                            , Column (Just <| rem operand 10) (Just 0)
                            ]
                        )
                        |> Expect.equal (initializeForModel Addition Model Column operand 0)
            , fuzz (intRange 10 99) "One operand is less than 100, the other is zero (subtraction)" <|
                \operand ->
                    Ok
                        (Model
                            [ Column (Just <| operand // 10) Nothing
                            , Column (Just <| rem operand 10) (Just 0)
                            ]
                        )
                        |> Expect.equal (initializeForModel Subtraction Model Column operand 0)
            , test "A 3-digit and a 2-digit integer (addition)" <|
                \() ->
                    Ok
                        (Model
                            [ Column Nothing Nothing
                            , Column (Just 1) Nothing
                            , Column (Just 2) (Just 7)
                            , Column (Just 3) (Just 5)
                            ]
                        )
                        |> Expect.equal (initializeForModel Addition Model Column 123 75)
            , test "A 3-digit and a 2-digit integer (subtraction)" <|
                \() ->
                    Ok
                        (Model
                            [ Column (Just 1) Nothing
                            , Column (Just 2) (Just 7)
                            , Column (Just 3) (Just 5)
                            ]
                        )
                        |> Expect.equal (initializeForModel Subtraction Model Column 123 75)
            , test "A 3-digit and a 5-digit integer (addition)" <|
                \() ->
                    Ok
                        (Model
                            [ Column Nothing Nothing
                            , Column Nothing (Just 7)
                            , Column Nothing (Just 5)
                            , Column (Just 3) (Just 4)
                            , Column (Just 2) (Just 3)
                            , Column (Just 1) (Just 2)
                            ]
                        )
                        |> Expect.equal (initializeForModel Addition Model Column 321 75432)
            , test "A 3-digit and a 5-digit integer (subtraction) is invalid" <|
                \() ->
                    (Err "First operand < second operand (321<75432)")
                        |> Expect.equal (initializeForModel Subtraction Model Column 321 75432)
            ]
        ]
