module AustrianSubtractionApp exposing (..)

import Html exposing (Html, div, input, section)
import Html.Attributes exposing (class, classList, value)
import Algorism.Common.Operator
import Algorism.Operands.Types
import Algorism.Operands.State
import Algorism.Operands.View
import Algorism.Subtraction.Austrian.Types
import Algorism.Subtraction.Austrian.State
import Algorism.Subtraction.Austrian.View
import Guarded.Input
import Guarded.Input.Parsers


type alias Operands =
    { firstOperand : Int
    , secondOperand : Int
    }


type alias Model =
    { inputModel : Algorism.Operands.Types.Model
    , maybeOperands : Maybe Operands
    , subtraction : Algorism.Subtraction.Austrian.Types.Model
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
    { inputModel = Algorism.Operands.State.initWith maxOperandDigits operandParser operandParser |> Algorism.Operands.State.withOperator Algorism.Common.Operator.Subtraction
    , maybeOperands = Nothing
    , subtraction = Algorism.Subtraction.Austrian.State.init
    }


type Msg
    = InputChanged Algorism.Operands.Types.Msg
    | SubtractionChanged Algorism.Subtraction.Austrian.Types.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        InputChanged operandsMsg ->
            let
                ( inputModel, subCmd ) =
                    Algorism.Operands.State.update operandsMsg model.inputModel

                ( maybeOperands, newSubtractionResult ) =
                    case (Algorism.Operands.State.operandsOf inputModel) of
                        Err err ->
                            ( Nothing, Err err )

                        Ok ( firstOperand, secondOperand ) ->
                            ( Just (Operands firstOperand secondOperand)
                            , Algorism.Subtraction.Austrian.Types.initializeFor firstOperand secondOperand
                            )

                newSubtraction =
                    case newSubtractionResult of
                        Err _ ->
                            model.subtraction

                        Ok subtraction ->
                            Algorism.Subtraction.Austrian.Types.solve subtraction
            in
                ( { model
                    | inputModel = inputModel
                    , maybeOperands = maybeOperands
                    , subtraction = newSubtraction
                  }
                , Cmd.map InputChanged subCmd
                )

        SubtractionChanged subtractionMsg ->
            let
                ( newSubtraction, subCmd ) =
                    Algorism.Subtraction.Austrian.State.update subtractionMsg model.subtraction
            in
                ( { model | subtraction = newSubtraction }
                , Cmd.map SubtractionChanged subCmd
                )


view : Model -> Html Msg
view model =
    section
        []
        [ div
            [ class "austrian-subtraction-section" ]
            [ Html.map InputChanged (Algorism.Operands.View.view model.inputModel)
            ]
        , div
            [ class "austrian-subtraction-section" ]
            [ Html.map SubtractionChanged (Algorism.Subtraction.Austrian.View.view model.subtraction)
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
