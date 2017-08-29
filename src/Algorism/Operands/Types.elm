module Algorism.Operands.Types exposing (..)

import Guarded.Input


type alias Model =
    { firstOperand : Guarded.Input.Model Int
    , secondOperand : Guarded.Input.Model Int
    }


type Msg
    = FirstOperandChanged (Guarded.Input.Msg Int)
    | SecondOperandChanged (Guarded.Input.Msg Int)


type Msg2Parent
    = ValidOperands ( Int, Int )
    | InvalidOperands
