module AdditionApp exposing (..)

import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (classList, disabled, value)
import Html.Events exposing (onClick)
import Algorism.Operands.Types
import Algorism.Operands.State
import Algorism.Operands.View
import Algorism.Addition.Types
import Algorism.Addition.State
import Algorism.Addition.View
import Guarded.Input
import Guarded.Input.Parsers


-- TODO either use CSS classes/colors, or discard them from index.html for now


type alias Operands =
    { firstOperand : Int
    , secondOperand : Int
    }


type alias Model =
    { inputModel : Algorism.Operands.Types.Model
    , maybeOperands : Maybe Operands
    , addition : Algorism.Addition.Types.Model
    }


maxOperand : Int
maxOperand =
    999


operandIntChecker : Int -> Result String Int
operandIntChecker =
    Guarded.Input.Parsers.positiveNumberChecker >> Result.andThen (Guarded.Input.Parsers.boundedNumberChecker (<=) maxOperand "Too big")


operandIntConverter : String -> Result String Int
operandIntConverter =
    Guarded.Input.Parsers.intConverter >> Result.andThen operandIntChecker


operandParser : String -> Guarded.Input.Msg Int
operandParser =
    Guarded.Input.parser operandIntConverter Guarded.Input.Parsers.nothingIsWorkInProgress


initialModel : Model
initialModel =
    { inputModel = Algorism.Operands.State.initWith operandParser operandParser
    , maybeOperands = Nothing
    , addition = Algorism.Addition.State.init
    }


type Msg
    = InputChanged Algorism.Operands.Types.Msg
    | AdditionChanged Algorism.Addition.Types.Msg
    | Calculate


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        InputChanged operandsMsg ->
            let
                ( inputModel, subCmd ) =
                    Algorism.Operands.State.update operandsMsg model.inputModel

                ( maybeOperands, newAdditionResult ) =
                    case (Algorism.Operands.State.operandsOf inputModel) of
                        Err err ->
                            ( Nothing, Err err )

                        Ok ( firstOperand, secondOperand ) ->
                            ( Just (Operands firstOperand secondOperand)
                            , Algorism.Addition.Types.initializeFor firstOperand secondOperand
                            )

                newAddition =
                    case newAdditionResult of
                        Err _ ->
                            model.addition

                        Ok addition ->
                            addition
            in
                ( { model
                    | inputModel = inputModel
                    , maybeOperands = maybeOperands
                    , addition = newAddition
                  }
                , Cmd.map InputChanged subCmd
                )

        AdditionChanged additionMsg ->
            let
                ( newAddition, subCmd ) =
                    Algorism.Addition.State.update additionMsg model.addition
            in
                ( { model | addition = newAddition }
                , Cmd.map AdditionChanged subCmd
                )

        Calculate ->
            let
                newAddition =
                    Algorism.Addition.Types.solve model.addition
            in
                ( { model | addition = newAddition }
                , Cmd.none
                )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Addition:"
            , Html.map InputChanged (Algorism.Operands.View.view model.inputModel)
            ]
        , button
            [ onClick Calculate
            , disabled (not <| hasValidOperands model)
            ]
            [ text "Calculate" ]
        , div
            []
            [ Html.map AdditionChanged (Algorism.Addition.View.view model.addition)
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


hasValidOperands : Model -> Bool
hasValidOperands model =
    case model.maybeOperands of
        Just _ ->
            True

        Nothing ->
            False
