module Algorism.Operands.Types exposing (..)

import Guarded.Input
import Algorism.Common.Operator exposing (Operator)


type alias Model =
    { operandSize : Int
    , firstOperand : Guarded.Input.Model Int
    , secondOperand : Guarded.Input.Model Int
    , firstParser : String -> Guarded.Input.Msg Int
    , secondParser : String -> Guarded.Input.Msg Int
    , operator : Operator
    }


type Msg
    = FirstOperandChanged (Guarded.Input.Msg Int)
    | SecondOperandChanged (Guarded.Input.Msg Int)
