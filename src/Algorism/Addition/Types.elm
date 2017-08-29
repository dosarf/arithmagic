module Algorism.Addition.Types exposing (..)


type alias Column =
    { carry : Maybe Int
    , firstOperand : Maybe Int
    , secondOperand : Maybe Int
    , result : Maybe Int
    }



-- TODO implement guarded input controls in the digit inputs
-- TODO tidy up this Addition module, as it were a real module
-- TODO Algorism.Addition.Addition - really?
-- TODO do we need Model.error down there?
-- TODO hide implementation details?
-- TODO Addition operations (initFor, solve, etc) should take and give Model, instead of Lists of columns
-- TODO get rid of Maybe errors, wherever possible


type alias Model =
    { columns : List Column
    , error : Maybe String
    }


type Msg
    = None
