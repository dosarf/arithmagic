module AdditionApp exposing (..)

import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (classList, disabled, value)
import Html.Events exposing (onClick)
import Algorism.Operands.Types
import Algorism.Operands.State
import Algorism.Operands.View
import Algorism.Addition.Addition
import Algorism.Addition.Types
import Algorism.Addition.State
import Algorism.Addition.View


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


initialModel : Model
initialModel =
    { inputModel = Algorism.Operands.State.init
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
                ( inputModel, msg2Parent, subCmd ) =
                    Algorism.Operands.State.updateWithMsg2Parent operandsMsg model.inputModel

                maybeOperands =
                    case msg2Parent of
                        Algorism.Operands.Types.InvalidOperands ->
                            Nothing

                        Algorism.Operands.Types.ValidOperands ( firstOperand, secondOperand ) ->
                            Just (Operands firstOperand secondOperand)

                newAdditionResultColumns =
                    case msg2Parent of
                        Algorism.Operands.Types.InvalidOperands ->
                            Err "Invalid"

                        Algorism.Operands.Types.ValidOperands ( firstOperand, secondOperand ) ->
                            Algorism.Addition.Addition.initializeFor firstOperand secondOperand

                newAddition =
                    case newAdditionResultColumns of
                        Err _ ->
                            model.addition

                        Ok columns ->
                            Algorism.Addition.Types.Model columns Nothing
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
                solvedColumns =
                    Algorism.Addition.Addition.solve model.addition.columns
            in
                ( { model | addition = Algorism.Addition.Types.Model solvedColumns Nothing }
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
