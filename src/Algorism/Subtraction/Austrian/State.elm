module Algorism.Subtraction.Austrian.State exposing (..)

import Algorism.Subtraction.Austrian.Types exposing (Column, Model, Msg(..), DigitInfo, EditableRow(..), guardedInputMsgToMsgFunc, initializeFor, solve, initializeColumnFor)
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
                                updateColumn editableRow inputMsg column
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


updateColumn : EditableRow -> Guarded.Input.Msg Int -> Column -> ( Column, Cmd (Guarded.Input.Msg Int) )
updateColumn editableRow inputMsg column =
    case editableRow of
        Borrow ->
            let
                ( userBorrow, subCmd ) =
                    Guarded.Input.update inputMsg column.userBorrow
            in
                ( { column
                    | userBorrow = userBorrow
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
