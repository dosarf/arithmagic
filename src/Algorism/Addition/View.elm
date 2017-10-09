module Algorism.Addition.View exposing (..)

import Algorism.Addition.Types exposing (Column, Model, Msg, guardedInputMsgToMsg, UserRow(..))
import Html exposing (Html, button, div, input, table, text, tr, td)
import Html.Attributes exposing (class, classList, value)
import Guarded.Input
import Guarded.Input.Parsers


{-
   CSS classes: algorism-addition-table, algorism-addition-static-tr, algorism-addition-static-td,
   algorism-addition-editable-tr, algorism-addition-editable-td, algorism-addition-input-correct,
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


editableRowView : UserRow -> Algorism.Addition.Types.Model -> (Algorism.Addition.Types.Column -> Maybe Int) -> (Algorism.Addition.Types.Column -> Guarded.Input.Model Int) -> Html Msg
editableRowView userRow addition solutionSelector userInputSelector =
    tr [ class "algorism-addition-editable-tr" ]
        (List.append
            [ td
                [ class "algorism-addition-editable-td"
                ]
                []
            ]
            (List.indexedMap
                (\columnIndex column -> createInputTd userRow columnIndex (solutionSelector column) (userInputSelector column))
                addition.columns
            )
        )


createInputTd : UserRow -> Int -> Maybe Int -> Guarded.Input.Model Int -> Html Msg
createInputTd userRow columnIndex maybeDigit userInput =
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
                [ Guarded.Input.parseOnInput (guardedInputMsgToMsg userRow columnIndex) Guarded.Input.Parsers.decimalDigitParser
                , value <| Guarded.Input.inputString userInput
                , class stateClass
                ]
                []
            ]
