module Algorism.Operands.View exposing (..)

import Algorism.Common.Operator exposing (Operator(..))
import Algorism.Operands.Types exposing (Model, Msg(..))
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (value, size)
import Guarded.Input
import Guarded.Input.Parsers


view : Model -> Html Msg
view model =
    div []
        [ input
            [ Guarded.Input.parseOnInput FirstOperandChanged model.firstParser
            , size model.operandSize
            , value <| Guarded.Input.inputString model.firstOperand
            ]
            []
        , text <| operatorView model.operator
        , input
            [ Guarded.Input.parseOnInput SecondOperandChanged model.secondParser
            , size model.operandSize
            , value <| Guarded.Input.inputString model.secondOperand
            ]
            []
        ]


operatorView : Operator -> String
operatorView operator =
    case operator of
        Addition ->
            "+"

        Subtraction ->
            "-"
