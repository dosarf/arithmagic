module Algorism.Subtraction.Standard.View exposing (..)

import Algorism.Subtraction.Standard.Types exposing (Column, Model, Msg, guardedInputMsgToMsg, boolInputMsgToMsg, IntUserRow(..), BoolUserRow(..))
import Html exposing (Html, button, div, input, span, table, text, tr, td)
import Html.Attributes exposing (class, classList, value)
import Html.Events exposing (onDoubleClick)
import Guarded.Input
import Guarded.Input.Parsers


{-
   TODO document these CSS classes:
   algorism-subtr-std-table,

   algorism-subtr-std-borrowed-from-outer, algorism-subtr-std-borrowed-from-inner,

   algorism-subtr-std-input-correct, algorism-std-austr-input-incorrect,
   algorism-subtr-std-input-unnecessary, algorism-std-austr-input-missing,
   algorism-subtr-std-input-correct-empty,

   algorism-subtr-std-borrow-correct, algorism-subtr-std-borrow-missing,
   algorism-subtr-std-borrow-unnecessary, algorism-subtr-std-borrow-correct-missing

   algorism-subtr-std-rg1st-op-tr, algorism-subtr-std-rg1st-op-td, algorism-subtr-std-rg1st-op-input

   algorism-subtr-std-1st-op-tr, algorism-subtr-std-1st-op-td

   algorism-subtr-std-2nd-op-tr, algorism-subtr-std-2nd-op-td

   algorism-subtr-std-result-tr, algorism-subtr-std-result-td, algorism-subtr-std-result-input

-}


view : Model -> Html Msg
view model =
    table
        [ class "algorism-subtr-std-table" ]
        [ regrouppedFirstOperandTr model
        , firstOperandTr model
        , secondOperandTr "-" model .secondOperand
        , resultTr model
        ]


regrouppedFirstOperandTr : Algorism.Subtraction.Standard.Types.Model -> Html Msg
regrouppedFirstOperandTr subtraction =
    tr [ class "algorism-subtr-std-rg1st-op-tr" ]
        (List.append
            [ td [] [] ]
            (List.indexedMap
                (\columnIndex column -> regrouppedFirstOperandTd columnIndex column)
                subtraction.columns
            )
        )


regrouppedFirstOperandTd : Int -> Algorism.Subtraction.Standard.Types.Column -> Html Msg
regrouppedFirstOperandTd columnIndex column =
    let
        correctnessClass =
            userInputCorrectnessClass column.regrouppedFirstOperand column.userRegrouppedFirstOperand

        borrowCorrectnessClass =
            userInputBorrowCorrectnessClass column.borrowFromRegrouppedFirstOperand column.userBorrowFromRegrouppedFirstOperand

        userRegrouppedFirstOperand =
            Guarded.Input.inputString column.userRegrouppedFirstOperand
    in
        case ( userRegrouppedFirstOperand, column.userBorrowFromRegrouppedFirstOperand ) of
            ( "", _ ) ->
                td
                    [ class "algorism-subtr-std-rg1st-op-td" ]
                    [ input
                        [ Guarded.Input.parseOnInput (guardedInputMsgToMsg RegrouppedFirstOperand columnIndex) undevigintiParser
                        , value ""
                        , classList
                            [ ( "algorism-subtr-std-rg1st-op-input", True )
                            , ( correctnessClass, True )
                            ]
                        ]
                        []
                    ]

            ( _, False ) ->
                td
                    [ classList
                        [ ( "algorism-subtr-std-rg1st-op-td", True )
                        , ( borrowCorrectnessClass, True )
                        ]
                    , onDoubleClick <| boolInputMsgToMsg BorrowFromRegrouppedFirstOperand columnIndex
                    ]
                    [ input
                        [ Guarded.Input.parseOnInput (guardedInputMsgToMsg RegrouppedFirstOperand columnIndex) undevigintiParser
                        , value userRegrouppedFirstOperand
                        , classList
                            [ ( "algorism-subtr-std-rg1st-op-input", True )
                            , ( correctnessClass, True )
                            ]
                        ]
                        []
                    ]

            ( _, True ) ->
                td
                    [ classList
                        [ ( "algorism-subtr-std-rg1st-op-td", True )
                        , ( borrowCorrectnessClass, True )
                        ]
                    , onDoubleClick <| boolInputMsgToMsg BorrowFromRegrouppedFirstOperand columnIndex
                    ]
                    [ input
                        [ Guarded.Input.parseOnInput (guardedInputMsgToMsg RegrouppedFirstOperand columnIndex) undevigintiParser
                        , value userRegrouppedFirstOperand
                        , classList
                            [ ( "algorism-subtr-std-rg1st-op-input", True )
                            , ( correctnessClass, True )
                            , ( "algorism-subtr-std-borrowed-from-input", True )
                            ]
                        ]
                        []
                    ]


firstOperandTr : Algorism.Subtraction.Standard.Types.Model -> Html Msg
firstOperandTr subtraction =
    tr [ class "algorism-subtr-std-1st-op-tr" ]
        (List.append
            [ td [] [] ]
            (List.indexedMap
                (\columnIndex column -> firstOperandTd columnIndex column)
                subtraction.columns
            )
        )


firstOperandTd : Int -> Algorism.Subtraction.Standard.Types.Column -> Html Msg
firstOperandTd columnIndex column =
    let
        borrowCorrectnessClass =
            userInputBorrowCorrectnessClass column.borrowFromFirstOperand column.userBorrowFromFirstOperand
    in
        case ( column.firstOperand, column.userBorrowFromFirstOperand ) of
            -- TODO consider first operand as Int, not as Maybe Int
            ( Nothing, _ ) ->
                td [ class "algorism-subtr-std-1st-op-td" ] []

            ( Just n, False ) ->
                td
                    [ classList
                        [ ( "algorism-subtr-std-1st-op-td", True )
                        , ( borrowCorrectnessClass, True )
                        ]
                    , onDoubleClick <| boolInputMsgToMsg BorrowFromFirstOperand columnIndex
                    ]
                    [ text <| toString n ]

            ( Just n, True ) ->
                td
                    [ classList
                        [ ( "algorism-subtr-std-1st-op-td", True )
                        , ( "algorism-subtr-std-borrowed-from-outer", True )
                        , ( borrowCorrectnessClass, True )
                        ]
                    , onDoubleClick <| boolInputMsgToMsg BorrowFromFirstOperand columnIndex
                    ]
                    [ span
                        [ class "algorism-subtr-std-borrowed-from-inner" ]
                        [ text <| toString n ]
                    ]


secondOperandTr : String -> Algorism.Subtraction.Standard.Types.Model -> (Algorism.Subtraction.Standard.Types.Column -> Maybe Int) -> Html Msg
secondOperandTr firstField subtraction selector =
    tr [ class "algorism-subtr-std-2nd-op-tr" ]
        (List.append
            [ td [] [ text firstField ] ]
            (List.map
                (\column -> selector column |> secondOperandTd)
                subtraction.columns
            )
        )


secondOperandTd : Maybe Int -> Html Msg
secondOperandTd maybeDigit =
    td
        [ class "algorism-subtr-std-2nd-op-td" ]
        [ text <| maybeDigitToBlankOrString maybeDigit ]


maybeDigitToBlankOrString : Maybe Int -> String
maybeDigitToBlankOrString maybeDigit =
    Maybe.map (\digit -> toString digit) maybeDigit |> Maybe.withDefault ""


resultTr : Algorism.Subtraction.Standard.Types.Model -> Html Msg
resultTr subtraction =
    tr [ class "algorism-subtr-std-result-tr" ]
        (List.append
            [ td [] [] ]
            (List.indexedMap
                (\index column -> resultTd index column.result column.userResult)
                subtraction.columns
            )
        )



-- TODO pass the column, more uniform handling


resultTd : Int -> Maybe Int -> Guarded.Input.Model Int -> Html Msg
resultTd columnIndex maybeResult userResult =
    let
        correctnessClass =
            userInputCorrectnessClass maybeResult userResult
    in
        td
            [ classList [ ( "algorism-subtr-std-result-td", True ), ( correctnessClass, True ) ]
            ]
            [ input
                [ Guarded.Input.parseOnInput (guardedInputMsgToMsg Result columnIndex) Guarded.Input.Parsers.decimalDigitParser
                , value <| Guarded.Input.inputString userResult
                , classList
                    [ ( "algorism-subtr-std-result-input", True )
                    , ( correctnessClass, True )
                    ]
                ]
                []
            ]


userInputCorrectnessClass : Maybe Int -> Guarded.Input.Model Int -> String
userInputCorrectnessClass maybeDigit userInput =
    let
        userInputResult =
            Guarded.Input.toResult userInput
    in
        case ( maybeDigit, userInputResult ) of
            ( Just digit2, Ok digit1 ) ->
                if digit1 == digit2 then
                    "algorism-subtr-std-input-correct"
                else
                    "algorism-subtr-std-input-incorrect"

            ( Nothing, Ok digit1 ) ->
                "algorism-subtr-std-input-unnecessary"

            ( Just digit, Err _ ) ->
                "algorism-subtr-std-input-missing"

            ( Nothing, Err _ ) ->
                "algorism-subtr-std-input-correct-empty"


userInputBorrowCorrectnessClass : Bool -> Bool -> String
userInputBorrowCorrectnessClass borrow userBorrow =
    case ( borrow, userBorrow ) of
        ( True, True ) ->
            "algorism-subtr-std-borrow-correct"

        ( True, False ) ->
            "algorism-subtr-std-borrow-missing"

        ( False, True ) ->
            "algorism-subtr-std-borrow-unnecessary"

        ( False, False ) ->
            "algorism-subtr-std-borrow-correct-missing"



-- Parser and checker for integers between 0 and 19 (undeviginti)


undevigintiParser : String -> Guarded.Input.Msg Int
undevigintiParser =
    Guarded.Input.parser (Guarded.Input.Parsers.intConverter >> Result.andThen undevigintiChecker) Guarded.Input.Parsers.nothingIsWorkInProgress


undevigintiChecker : comparable -> Result String comparable
undevigintiChecker =
    Guarded.Input.Parsers.boundedNumberChecker (>=) 0 "Less than  0" >> Result.andThen (Guarded.Input.Parsers.boundedNumberChecker (<=) 19 "More than 19")
