module Algorism.Subtraction.Standard.State exposing (..)

import Algorism.Subtraction.Standard.Types exposing (..)
import Guarded.Input


-- TODO the columns list could be empty (also for Addition, and Subtraction.Austrian)


init : Model
init =
    { columns = [ initializeColumnFor Nothing Nothing ]
    }



-- TODO (not quite) similar to Algorism.Subtraction.update


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        UserIntInputChanged { userRow, columnIndex, inputMsg } ->
            let
                updatedColumnMsgTuples =
                    List.indexedMap
                        (\index column ->
                            if columnIndex == index then
                                updateIntColumn userRow inputMsg column
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
                , Cmd.map (guardedInputMsgToMsg userRow columnIndex) subCmd
                )

        UserBorrowToggled { userRow, columnIndex } ->
            let
                updatedColumns =
                    List.indexedMap
                        (\index column ->
                            if columnIndex == index then
                                updateBoolColumn userRow column
                            else
                                column
                        )
                        model.columns
            in
                ( { model | columns = updatedColumns }
                , Cmd.none
                )


updateIntColumn : IntUserRow -> Guarded.Input.Msg Int -> Column -> ( Column, Cmd (Guarded.Input.Msg Int) )
updateIntColumn userRow inputMsg column =
    case userRow of
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


updateBoolColumn : BoolUserRow -> Column -> Column
updateBoolColumn userRow column =
    case userRow of
        BorrowFromRegrouppedFirstOperand ->
            { column | userBorrowFromRegrouppedFirstOperand = not column.userBorrowFromRegrouppedFirstOperand }

        BorrowFromFirstOperand ->
            { column | userBorrowFromFirstOperand = not column.userBorrowFromFirstOperand }
