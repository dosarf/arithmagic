module Algorism.Subtraction.Standard.State exposing (..)

import Algorism.Subtraction.Standard.Types exposing (..)
import Guarded.Input


init : Model
init =
    { columns = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        DigitEdited { editableRow, columnIndex, inputMsg } ->
            let
                updatedColumnMsgTuples =
                    List.indexedMap
                        (\index column ->
                            if columnIndex == index then
                                updateIntColumn editableRow inputMsg column
                            else
                                ( column, Cmd.none )
                        )
                        model.columns

                updatedColumns =
                    List.map Tuple.first updatedColumnMsgTuples

                maybeSubCmd =
                    List.map Tuple.second updatedColumnMsgTuples |> List.filter (\cmd -> cmd /= Cmd.none) |> List.head

                subCmd =
                    Maybe.withDefault Cmd.none maybeSubCmd
            in
                ( { model | columns = updatedColumns }
                , Cmd.map (guardedInputMsgToMsgFunc editableRow columnIndex) subCmd
                )

        BoolToggled { editableRow, columnIndex } ->
            let
                updatedColumns =
                    List.indexedMap
                        (\index column ->
                            if columnIndex == index then
                                updateBoolColumn editableRow column
                            else
                                column
                        )
                        model.columns
            in
                ( { model | columns = updatedColumns }
                , Cmd.none
                )


updateIntColumn : EditableIntRow -> Guarded.Input.Msg Int -> Column -> ( Column, Cmd (Guarded.Input.Msg Int) )
updateIntColumn editableRow inputMsg column =
    case editableRow of
        RegrouppedFirstOperand ->
            let
                ( userRegrouppedFirstOperand, subCmd ) =
                    Guarded.Input.update inputMsg column.userRegrouppedFirstOperand
            in
                ( { column
                    | userRegrouppedFirstOperand = userRegrouppedFirstOperand
                  }
                , subCmd
                )

        Result ->
            let
                ( userResult, subCmd ) =
                    Guarded.Input.update inputMsg column.userResult
            in
                ( { column
                    | userResult = userResult
                  }
                , subCmd
                )


updateBoolColumn : EditableBoolRow -> Column -> Column
updateBoolColumn editableRow column =
    case editableRow of
        BorrowFromRegrouppedFirstOperand ->
            { column | userBorrowFromRegrouppedFirstOperand = not column.userBorrowFromRegrouppedFirstOperand }

        BorrowFromFirstOperand ->
            { column | userBorrowFromFirstOperand = not column.userBorrowFromFirstOperand }
