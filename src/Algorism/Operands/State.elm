module Algorism.Operands.State exposing (..)

import Guarded.Input
import Guarded.Input.Parsers
import Algorism.Common.Operator exposing (Operator(..))
import Algorism.Operands.Types exposing (Model, Msg(..))


init : Model
init =
    { firstOperand = Guarded.Input.init
    , secondOperand = Guarded.Input.init
    , firstParser = Guarded.Input.Parsers.nonNegativeIntParser
    , secondParser = Guarded.Input.Parsers.nonNegativeIntParser
    , operator = Addition
    }


initWith : (String -> Guarded.Input.Msg Int) -> (String -> Guarded.Input.Msg Int) -> Model
initWith firstParser secondParser =
    Model Guarded.Input.init Guarded.Input.init firstParser secondParser Addition


withOperator : Operator -> Model -> Model
withOperator operator model =
    { model | operator = operator }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        FirstOperandChanged msg ->
            let
                ( newOperand, subMsg ) =
                    Guarded.Input.update msg model.firstOperand
            in
                ( { model | firstOperand = newOperand }
                , Cmd.map FirstOperandChanged subMsg
                )

        SecondOperandChanged msg ->
            let
                ( newOperand, subMsg ) =
                    Guarded.Input.update msg model.secondOperand
            in
                ( { model | secondOperand = newOperand }
                , Cmd.map SecondOperandChanged subMsg
                )


operandsOf : Model -> Result String ( Int, Int )
operandsOf model =
    let
        firstOperandResult =
            Guarded.Input.toResult model.firstOperand

        secondOperandResult =
            Guarded.Input.toResult model.secondOperand
    in
        case ( firstOperandResult, secondOperandResult ) of
            ( Ok firstOperand, Ok secondOperand ) ->
                Ok ( firstOperand, secondOperand )

            _ ->
                Err "(some) operands unavailable"
