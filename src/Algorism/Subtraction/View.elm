module Algorism.Subtraction.View exposing (..)

import Algorism.Subtraction.Types exposing (Column, Model, Msg, guardedInputMsgToMsg, UserRow(..))
import Html exposing (Html, button, div, input, table, text, tr, td)
import Html.Attributes exposing (class, classList, value)
import Guarded.Input
import Guarded.Input.Parsers


{-
   TODO document these CSS classes:
   algorism-subtraction-table, algorism-subtraction-static-tr, algorism-subtraction-static-td,
   algorism-subtraction-editable-tr, algorism-subtraction-editable-td,
   algorism-subtraction-loan-tr, algorism-subtraction-loan-td,
   algorism-subtraction-input, algorism-subtraction-input-correct,
   algorism-subtraction-input-incorrect, algorism-subtraction-input-unnecessary, algorism-subtraction-input-missing,
   algorism-subtraction-input-correct-empty,
   algorism-subtraction-static-filler-td,
   algorism-subtraction-loan-filler-td
-}


view : Model -> Html Msg
view model =
    table
        [ class "algorism-subtraction-table" ]
        [ staticRowView "" model .firstOperand
        , loanRowView model
        , staticRowView "-" model .secondOperand
        , resultRowView model
        ]


staticRowView : String -> Algorism.Subtraction.Types.Model -> (Algorism.Subtraction.Types.Column -> Maybe Int) -> Html Msg
staticRowView firstField addition selector =
    tr [ class "algorism-subtraction-static-tr" ]
        (List.append
            [ td [] [ text firstField ] ]
            (List.concat
                (List.map
                    (\column -> selector column |> createTextTds)
                    addition.columns
                )
            )
        )


createTextTds : Maybe Int -> List (Html Msg)
createTextTds maybeDigit =
    [ td
        [ class "algorism-subtraction-static-td"
        ]
        [ maybeDigitToBlankOrString maybeDigit |> text ]
    , td [ class "algorism-subtraction-static-filler-td" ] []
    ]


maybeDigitToBlankOrString : Maybe Int -> String
maybeDigitToBlankOrString maybeDigit =
    Maybe.map (\digit -> toString digit) maybeDigit |> Maybe.withDefault ""


resultRowView : Algorism.Subtraction.Types.Model -> Html Msg
resultRowView subtraction =
    tr [ class "algorism-subtraction-editable-tr" ]
        (List.append
            [ td
                [ class "algorism-subtraction-editable-td"
                ]
                []
            ]
            (List.concat
                (List.indexedMap
                    (\columnIndex column -> createInputTds Result columnIndex column.result column.userResult)
                    subtraction.columns
                )
            )
        )


loanRowView : Algorism.Subtraction.Types.Model -> Html Msg
loanRowView subtraction =
    tr [ class "algorism-subtraction-loan-tr" ]
        (List.append
            [ td
                [ class "algorism-subtraction-loan-td"
                ]
                []
            ]
            (List.concat
                (List.indexedMap
                    (\columnIndex column -> createInputTds Loan columnIndex column.loan column.userLoan)
                    subtraction.columns
                )
            )
        )


createInputTds : UserRow -> Int -> Maybe Int -> Guarded.Input.Model Int -> List (Html Msg)
createInputTds userRow columnIndex maybeDigit userInput =
    let
        userInputResult =
            Guarded.Input.toResult userInput

        stateClass =
            case ( userInputResult, maybeDigit ) of
                ( Ok digit1, Just digit2 ) ->
                    if digit1 == digit2 then
                        "algorism-subtraction-input-correct"
                    else
                        "algorism-subtraction-input-incorrect"

                ( Ok digit1, Nothing ) ->
                    "algorism-subtraction-input-unnecessary"

                ( Err _, Just digit ) ->
                    "algorism-subtraction-input-missing"

                ( Err _, Nothing ) ->
                    "algorism-subtraction-input-correct-empty"

        tdClass =
            if userRow == Loan then
                "algorism-subtraction-loan-td"
            else
                "algorism-subtraction-editable-td"

        tData =
            td
                [ classList
                    [ ( tdClass, True )
                    , ( stateClass, True )
                    ]
                ]
                [ input
                    [ Guarded.Input.parseOnInput (guardedInputMsgToMsg userRow columnIndex) Guarded.Input.Parsers.decimalDigitParser
                    , value <| Guarded.Input.inputString userInput
                    , classList
                        [ ( "algorism-subtraction-input", True )
                        , ( stateClass, True )
                        ]
                    ]
                    []
                ]
    in
        case userRow of
            Result ->
                [ tData
                , td [ class "algorism-subtraction-static-filler-td" ] []
                ]

            Loan ->
                [ td [ class "algorism-subtraction-loan-filler-td" ] []
                , tData
                ]
