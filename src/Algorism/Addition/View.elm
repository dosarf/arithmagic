module Algorism.Addition.View exposing (..)

import Algorism.Addition.Types exposing (Column, Model, Msg)
import Html exposing (Html, button, div, input, table, text, tr, td)
import Html.Attributes exposing (placeholder, value)


view : Model -> Html Msg
view model =
    case model.error of
        Just errorMessage ->
            "Oops: " ++ errorMessage |> text

        Nothing ->
            table
                []
                [ rowView "" model.addition .carry createInputTd
                , rowView "" model.addition .firstOperand createTextTd
                , rowView "+" model.addition .secondOperand createTextTd
                , rowView "" model.addition .result createInputTd
                ]


rowView : String -> Algorism.Addition.Model -> (Algorism.Addition.Column -> Maybe Int) -> (Maybe Int -> Html Msg) -> Html Msg
rowView firstField addition rowSelector tdContentCreator =
    tr []
        (List.append
            [ td [] [ text firstField ] ]
            (List.map
                (\column -> rowSelector column |> tdContentCreator)
                addition.columns
            )
        )


createInputTd : Maybe Int -> Html Msg
createInputTd maybeDigit =
    td [] [ input [ maybeDigitToBlankOrString maybeDigit |> value ] [] ]


createTextTd : Maybe Int -> Html Msg
createTextTd maybeDigit =
    td [] [ maybeDigitToBlankOrString maybeDigit |> text ]


maybeDigitToBlankOrString : Maybe Int -> String
maybeDigitToBlankOrString maybeDigit =
    Maybe.map (\digit -> toString digit) maybeDigit |> Maybe.withDefault ""
