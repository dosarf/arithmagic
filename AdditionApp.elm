module AdditionApp exposing (..)

import Html exposing (Html, div, input, section)
import Html.Attributes exposing (class, classList, value)
import Algorism.Operands.Types
import Algorism.Operands.State
import Algorism.Operands.View
import Algorism.Addition.Types
import Algorism.Addition.State
import Algorism.Addition.View
import Guarded.Input
import Guarded.Input.Parsers


type alias Operands =
    { firstOperand : Int
    , secondOperand : Int
    }


type alias Model =
    { inputModel : Algorism.Operands.Types.Model
    , maybeOperands : Maybe Operands
    , addition : Algorism.Addition.Types.Model
    }


maxOperandDigits : Int
maxOperandDigits =
    4


maxOperand : Int
maxOperand =
    10 ^ maxOperandDigits - 1


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
    { inputModel = Algorism.Operands.State.initWith maxOperandDigits operandParser operandParser
    , maybeOperands = Nothing
    , addition = Algorism.Addition.State.init
    }


type Msg
    = InputChanged Algorism.Operands.Types.Msg
    | AdditionChanged Algorism.Addition.Types.Msg


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
                            Algorism.Addition.Types.solve addition
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


view : Model -> Html Msg
view model =
    section
        []
        [ div
            [ class "addition-section" ]
            [ Html.map InputChanged (Algorism.Operands.View.view model.inputModel)
            ]
        , div
            [ class "addition-section" ]
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
