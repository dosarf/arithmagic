module Algorism.Addition.Types exposing (..)


type alias Column =
    { carry : Maybe Int
    , firstOperand : Maybe Int
    , secondOperand : Maybe Int
    , result : Maybe Int
    }


type alias Model =
    { columns : List Column
    , error : Maybe String
    }


type Msg
    = InitializeFor ( Int, Int )
    | Solve
