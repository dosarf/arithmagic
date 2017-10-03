module Algorism.Operands.View exposing (..)

import Algorism.Operands.Types exposing (Model, Msg(..))
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (value)
import Guarded.Input
import Guarded.Input.Parsers


view : Model -> Html Msg
view model =
    div []
        [ input
            [ Guarded.Input.parseOnInput FirstOperandChanged model.firstParser
            , value <| Guarded.Input.inputString model.firstOperand
            ]
            []
        , text "+"
        , input
            [ Guarded.Input.parseOnInput SecondOperandChanged model.secondParser
            , value <| Guarded.Input.inputString model.secondOperand
            ]
            []
        ]
