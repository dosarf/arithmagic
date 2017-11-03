module Algorism.Subtraction.Austrian.View exposing (..)

import Algorism.Subtraction.Austrian.Types exposing (Column, Model, Msg, guardedInputMsgToMsgFunc, EditableRow(..))
import Html exposing (Html, button, div, input, table, text, tr, td)
import Html.Attributes exposing (class, classList, value, rowspan, colspan)
import Guarded.Input
import Guarded.Input.Parsers


{-
   TODO document these CSS classes:
   algorism-subtr-austr-table,
   algorism-subtr-austr-operand-tr,
   algorism-subtr-austr-right-filler-td, algorism-subtr-austr-operand-td, algorism-subtr-austr-left-filler-td,

   algorism-subtr-austr-borrow-tr, algorism-subtr-austr-first-operand-bottom-filler-td, algorism-subtr-austr-borrow-td,

   algorism-subtr-austr-borrow-td, algorism-subtr-austr-borrow-input,

   algorism-subtr-austr-input-correct, algorism-subtr-austr-input-incorrect,
   algorism-subtr-austr-input-unnecessary, algorism-subtr-austr-input-missing,
   algorism-subtr-austr-input-correct-empty,


   algorism-subtr-austr-bottom-borrow-tr, algorism-subtr-austr-second-operand-top-filler-td,

   algorism-subtr-austr-result-tr,

   algorism-subtr-austr-result-input, algorism-subtr-austr-result-td
-}


view : Model -> Html Msg
view model =
    table
        [ class "algorism-subtr-austr-table" ]
        [ operandTr "" model .firstOperand
        , borrowTr model
        , bottomBorrowTr model
        , operandTr "-" model .secondOperand
        , resultTr model
        ]


operandTr : String -> Algorism.Subtraction.Austrian.Types.Model -> (Algorism.Subtraction.Austrian.Types.Column -> Maybe Int) -> Html Msg
operandTr firstField addition selector =
    let
        columnCount =
            List.length addition.columns

        columnPositionFor =
            columnPosition columnCount
    in
        tr [ class "algorism-subtr-austr-operand-tr" ]
            ((td [] [ text firstField ])
                :: (List.concat
                        (List.indexedMap
                            (\columnIndex column -> operandTds (selector column) (columnPositionFor columnIndex))
                            addition.columns
                        )
                   )
            )


type ColumnPosition
    = MostSignificant
    | Middle
    | LeastSignificant


columnPosition : Int -> Int -> ColumnPosition
columnPosition columnCount columnIndex =
    case ( columnIndex, columnCount - 1 - columnIndex ) of
        ( 0, _ ) ->
            MostSignificant

        ( _, 0 ) ->
            LeastSignificant

        _ ->
            Middle


operandTds : Maybe Int -> ColumnPosition -> List (Html Msg)
operandTds maybeDigit columnPosition =
    case columnPosition of
        MostSignificant ->
            [ operandTd maybeDigit
            , td [ class "algorism-subtr-austr-right-filler-td" ] []
            ]

        Middle ->
            [ td [ class "algorism-subtr-austr-left-filler-td" ] []
            , operandTd maybeDigit
            , td [ class "algorism-subtr-austr-right-filler-td" ] []
            ]

        LeastSignificant ->
            [ td [ class "algorism-subtr-austr-left-filler-td" ] []
            , operandTd maybeDigit
            ]


operandTd : Maybe Int -> Html Msg
operandTd maybeDigit =
    td [ class "algorism-subtr-austr-operand-td" ] [ text <| maybeDigitToBlankOrString maybeDigit ]


maybeDigitToBlankOrString : Maybe Int -> String
maybeDigitToBlankOrString maybeDigit =
    Maybe.map (\digit -> toString digit) maybeDigit |> Maybe.withDefault ""


borrowTr : Algorism.Subtraction.Austrian.Types.Model -> Html Msg
borrowTr addition =
    let
        columnCount =
            List.length addition.columns

        columnPositionFor =
            columnPosition columnCount
    in
        tr
            [ class "algorism-subtr-austr-borrow-tr" ]
            ((td [] [])
                :: (List.concat
                        (List.indexedMap
                            (\columnIndex column -> borrowTds column.borrow column.userBorrow columnIndex (columnPositionFor columnIndex))
                            addition.columns
                        )
                   )
            )


borrowTds : Maybe Int -> Guarded.Input.Model Int -> Int -> ColumnPosition -> List (Html Msg)
borrowTds maybeBorrow userBorrow columnIndex columnPosition =
    let
        correctnessClass =
            userInputCorrectnessClass maybeBorrow userBorrow
    in
        if columnPosition == LeastSignificant then
            [ td [ class "algorism-subtr-austr-first-operand-bottom-filler-td" ] []
            ]
        else
            [ td [ class "algorism-subtr-austr-first-operand-bottom-filler-td" ] []
            , td
                [ classList [ ( "algorism-subtr-austr-borrow-td", True ), ( correctnessClass, True ) ]
                , rowspan 2
                , colspan 2
                ]
                [ input
                    [ Guarded.Input.parseOnInput (guardedInputMsgToMsgFunc Borrow columnIndex) Guarded.Input.Parsers.decimalDigitParser
                    , value <| Guarded.Input.inputString userBorrow
                    , classList
                        [ ( "algorism-subtr-austr-borrow-input", True )
                        , ( correctnessClass, True )
                        ]
                    ]
                    []
                ]
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
                    "algorism-subtr-austr-input-correct"
                else
                    "algorism-subtr-austr-input-incorrect"

            ( Nothing, Ok digit1 ) ->
                "algorism-subtr-austr-input-unnecessary"

            ( Just digit, Err _ ) ->
                "algorism-subtr-austr-input-missing"

            ( Nothing, Err _ ) ->
                "algorism-subtr-austr-input-correct-empty"


bottomBorrowTr : Algorism.Subtraction.Austrian.Types.Model -> Html Msg
bottomBorrowTr addition =
    let
        columnCount =
            List.length addition.columns
    in
        tr
            [ class "algorism-subtr-austr-bottom-borrow-tr" ]
            (List.repeat
                columnCount
                bottomBorrowTd
            )


bottomBorrowTd : Html Msg
bottomBorrowTd =
    td [ class "algorism-subtr-austr-second-operand-top-filler-td" ] []


resultTr : Algorism.Subtraction.Austrian.Types.Model -> Html Msg
resultTr addition =
    let
        columnCount =
            List.length addition.columns

        columnPositionFor =
            columnPosition columnCount
    in
        tr [ class "algorism-subtr-austr-result-tr" ]
            ((td [] [])
                :: (List.concat
                        (List.indexedMap
                            (\columnIndex column -> resultTds column.result column.userResult columnIndex (columnPositionFor columnIndex))
                            addition.columns
                        )
                   )
            )


resultTds : Maybe Int -> Guarded.Input.Model Int -> Int -> ColumnPosition -> List (Html Msg)
resultTds maybeResult userResult columnIndex columnPosition =
    let
        correctnessClass =
            userInputCorrectnessClass maybeResult userResult
    in
        case columnPosition of
            MostSignificant ->
                [ resultTd correctnessClass columnIndex maybeResult userResult
                , td [ class "algorism-subtr-austr-right-filler-td" ] []
                ]

            Middle ->
                [ td [ class "algorism-subtr-austr-left-filler-td" ] []
                , resultTd correctnessClass columnIndex maybeResult userResult
                , td [ class "algorism-subtr-austr-right-filler-td" ] []
                ]

            LeastSignificant ->
                [ td [ class "algorism-subtr-austr-left-filler-td" ] []
                , resultTd correctnessClass columnIndex maybeResult userResult
                ]


resultTd : String -> Int -> Maybe Int -> Guarded.Input.Model Int -> Html Msg
resultTd correctnessClass columnIndex maybeResult userResult =
    td
        [ classList [ ( "algorism-subtr-austr-result-td", True ), ( correctnessClass, True ) ]
        ]
        [ input
            [ Guarded.Input.parseOnInput (guardedInputMsgToMsgFunc Result columnIndex) Guarded.Input.Parsers.decimalDigitParser
            , value <| Guarded.Input.inputString userResult
            , classList
                [ ( "algorism-subtr-austr-result-input", True )
                , ( correctnessClass, True )
                ]
            ]
            []
        ]
