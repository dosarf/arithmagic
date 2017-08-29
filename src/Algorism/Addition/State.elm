module Algorism.Addition.State exposing (..)

import Algorism.Addition.Types exposing (Column, Model, Msg(..))
import Algorism.Addition.Addition exposing (initializeFor, solve, initializeColumnFor)


init : Model
init =
    { columns = [ initializeColumnFor Nothing Nothing ]
    , error = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        None ->
            ( model
            , Cmd.none
            )
