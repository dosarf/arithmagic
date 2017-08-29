module Algorism.Operands.State exposing (..)

import Guarded.Input
import Algorism.Operands.Types exposing (Model, Msg(..), Msg2Parent(..))


-- TODO do Operands really need message to the parent?
-- TODO allow initialization for inputs with boundaries
-- TODO get rid of Maybe errors, wherever possible


init : Model
init =
    { firstOperand = Guarded.Input.init
    , secondOperand = Guarded.Input.init
    }


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



-- API for a parent component to learn about valid operands


updateWithMsg2Parent : Msg -> Model -> ( Model, Msg2Parent, Cmd Msg )
updateWithMsg2Parent message model =
    let
        ( newModel, cmd ) =
            update message model
    in
        ( newModel, model2OutMsg newModel, cmd )


model2OutMsg : Model -> Msg2Parent
model2OutMsg model =
    let
        firstOperandResult =
            Guarded.Input.toResult model.firstOperand

        secondOperandResult =
            Guarded.Input.toResult model.secondOperand
    in
        case ( firstOperandResult, secondOperandResult ) of
            ( Ok firstOperand, Ok secondOperand ) ->
                ValidOperands ( firstOperand, secondOperand )

            _ ->
                InvalidOperands
