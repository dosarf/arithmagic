module Algorism.Addition.View exposing (..)

import Algorism.Addition.Types exposing (Column, Model, Msg, guardedInputMsgToMsgFunc, EditableRow(..))
import Html exposing (Html, button, div, input, table, text, tr, td)
import Html.Attributes exposing (class, classList, value)
import Guarded.Input
import Guarded.Input.Parsers


{-
   TODO document these CSS classes:
   algorism-addition-table, algorism-addition-static-tr, algorism-addition-static-td,
   algorism-addition-carry-tr,
   algorism-addition-result-tr,
   algorism-addition-editable-td,
   algorism-addition-input, algorism-addition-input-correct,
   algorism-addition-input-incorrect, algorism-addition-input-unnecessary, algorism-addition-input-missing,
   algorism-addition-input-correct-empty
-}


view : Model -> Html Msg
view model =
    table
        [ class "algorism-addition-table" ]
        [ editableRowView Carry model .carry .userCarry
        , staticRowView "" model .firstOperand
        , staticRowView "+" model .secondOperand
        , editableRowView Result model .result .userResult
        ]


staticRowView : String -> Algorism.Addition.Types.Model -> (Algorism.Addition.Types.Column -> Maybe Int) -> Html Msg
staticRowView firstField addition selector =
    tr [ class "algorism-addition-static-tr" ]
        (List.append
            [ td [] [ text firstField ] ]
            (List.map
                (\column -> selector column |> createTextTd)
                addition.columns
            )
        )


createTextTd : Maybe Int -> Html Msg
createTextTd maybeDigit =
    td
        [ class "algorism-addition-static-td"
        ]
        [ maybeDigitToBlankOrString maybeDigit |> text ]


maybeDigitToBlankOrString : Maybe Int -> String
maybeDigitToBlankOrString maybeDigit =
    Maybe.map (\digit -> toString digit) maybeDigit |> Maybe.withDefault ""


editableRowView : EditableRow -> Algorism.Addition.Types.Model -> (Algorism.Addition.Types.Column -> Maybe Int) -> (Algorism.Addition.Types.Column -> Guarded.Input.Model Int) -> Html Msg
editableRowView editableRow addition solutionSelector userInputSelector =
    let
        rowCssClass =
            case editableRow of
                Carry ->
                    "algorism-addition-carry-tr"

                Result ->
                    "algorism-addition-result-tr"
    in
        tr [ class rowCssClass ]
            (List.append
                [ td [] []
                ]
                (List.indexedMap
                    (\columnIndex column -> createInputTd editableRow columnIndex (solutionSelector column) (userInputSelector column))
                    addition.columns
                )
            )


createInputTd : EditableRow -> Int -> Maybe Int -> Guarded.Input.Model Int -> Html Msg
createInputTd editableRow columnIndex maybeDigit userInput =
    let
        userInputResult =
            Guarded.Input.toResult userInput

        stateClass =
            case ( userInputResult, maybeDigit ) of
                ( Ok digit1, Just digit2 ) ->
                    if digit1 == digit2 then
                        "algorism-addition-input-correct"
                    else
                        "algorism-addition-input-incorrect"

                ( Ok digit1, Nothing ) ->
                    "algorism-addition-input-unnecessary"

                ( Err _, Just digit ) ->
                    "algorism-addition-input-missing"

                ( Err _, Nothing ) ->
                    "algorism-addition-input-correct-empty"
    in
        td
            [ classList
                [ ( "algorism-addition-editable-td", True )
                , ( stateClass, True )
                ]
            ]
            [ input
                [ Guarded.Input.parseOnInput (guardedInputMsgToMsgFunc editableRow columnIndex) Guarded.Input.Parsers.decimalDigitParser
                , value <| Guarded.Input.inputString userInput
                , classList
                    [ ( "algorism-addition-input", True )
                    , ( stateClass, True )
                    ]
                ]
                []
            ]
