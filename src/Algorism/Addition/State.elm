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
        InitializeFor ( firstOperand, secondOperand ) ->
            let
                initializationResult =
                    initializeFor firstOperand secondOperand
            in
                case initializationResult of
                    Err error ->
                        ( { columns = [ initializeColumnFor Nothing Nothing ]
                          , error = Just error
                          }
                        , Cmd.none
                        )

                    Ok columns ->
                        ( { columns = columns
                          , error = Nothing
                          }
                        , Cmd.none
                        )

        Solve ->
            ( solveValidModel model
            , Cmd.none
            )


solveValidModel : Model -> Model
solveValidModel { columns, error } =
    case error of
        Nothing ->
            Model (solve columns) Nothing

        Just _ ->
            Model columns error
