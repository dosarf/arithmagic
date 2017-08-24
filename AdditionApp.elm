module Main exposing (..)

import Html exposing (Html, button, div, input, table, text, tr, td)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import String
import Algorism.Addition exposing (initModel, initModelFor, solve, Column)


type alias Model =
    { firstOperand : Maybe Int
    , secondOperand : Maybe Int
    , error : Maybe String
    , addition : Algorism.Addition.Model
    }


initialModel : Model
initialModel =
    { firstOperand = Nothing
    , secondOperand = Nothing
    , error = Nothing
    , addition = initModel
    }


type IntOperandChangeInfo
    = Undefined
    | InvalidInt ( String, String )
    | ValidInt Int


type Msg
    = FirstOperandChanged IntOperandChangeInfo
    | SecondOperandChanged IntOperandChangeInfo
    | Calculate
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        FirstOperandChanged Undefined ->
            ( { model
                | firstOperand = Nothing
                , error = Nothing
              }
            , Cmd.none
            )

        FirstOperandChanged (ValidInt firstOperand) ->
            ( { model
                | firstOperand = Just firstOperand
                , error = Nothing
              }
            , Cmd.none
            )

        FirstOperandChanged (InvalidInt ( string, error )) ->
            ( { model
                | firstOperand = model.firstOperand
                , error = Just error
              }
            , Cmd.none
            )

        SecondOperandChanged Undefined ->
            ( { model
                | secondOperand = Nothing
                , error = Nothing
              }
            , Cmd.none
            )

        SecondOperandChanged (ValidInt secondOperand) ->
            ( { model
                | secondOperand = Just secondOperand
                , error = Nothing
              }
            , Cmd.none
            )

        SecondOperandChanged (InvalidInt ( string, error )) ->
            ( { model
                | secondOperand = model.secondOperand
                , error = Just error
              }
            , Cmd.none
            )

        Calculate ->
            let
                firstOperand =
                    Maybe.withDefault 0 model.firstOperand

                secondOperand =
                    Maybe.withDefault 0 model.secondOperand

                additionResult =
                    initModelFor firstOperand secondOperand
            in
                case additionResult of
                    Ok addition ->
                        ( { model
                            | addition = solve addition
                            , error = Nothing
                          }
                        , Cmd.none
                        )

                    Err error ->
                        ( { model
                            | addition = initModel
                            , error = Just error
                          }
                        , Cmd.none
                        )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Addition:"
            , input
                [ onInput (string2IntOperandChangeInfo >> FirstOperandChanged)
                , value (operandToString model.firstOperand)
                ]
                []
            , text "+"
            , input
                [ onInput (string2IntOperandChangeInfo >> SecondOperandChanged)
                , value (operandToString model.secondOperand)
                ]
                []
            ]
        , button [ onClick Calculate ] [ text "Calculate" ]
        , solutionView model
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


operandToString : Maybe Int -> String
operandToString maybeOperand =
    case maybeOperand of
        Nothing ->
            ""

        Just operand ->
            toString operand


string2IntOperandChangeInfo : String -> IntOperandChangeInfo
string2IntOperandChangeInfo string =
    if string == "" then
        Undefined
    else
        let
            operandResult =
                String.toInt string
        in
            case operandResult of
                Ok operand ->
                    ValidInt operand

                Err error ->
                    InvalidInt ( string, error )
