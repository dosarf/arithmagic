module Algorism.Subtraction.Austrian.State exposing (..)

import Algorism.Subtraction.Austrian.Types exposing (Column, Model, Msg(..), UserInputMsg, UserRow(..), guardedInputMsgToMsg, initializeFor, solve, initializeColumnFor)
import Guarded.Input


init : Model
init =
    { columns = [ initializeColumnFor Nothing Nothing ]
    }



-- TODO this is a verbatim copy of Algorism.Addition.update


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        UserInputChanged { userRow, columnIndex, inputMsg } ->
            let
                updatedColumnMsgTuples =
                    List.indexedMap
                        (\index column ->
                            if columnIndex == index then
                                updateColumn userRow inputMsg column
                            else
                                ( column, Cmd.none )
                        )
                        model.columns

                updatedColumns =
                    List.map Tuple.first updatedColumnMsgTuples

                subCmd =
                    List.map Tuple.second updatedColumnMsgTuples |> List.filter (\cmd -> cmd /= Cmd.none) |> List.head |> Maybe.withDefault Cmd.none
            in
                ( { model | columns = updatedColumns }
                , Cmd.map (guardedInputMsgToMsg userRow columnIndex) subCmd
                )



-- TODO this is almost a verbatim copy of Algorism.Addition.updateColumn


updateColumn : UserRow -> Guarded.Input.Msg Int -> Column -> ( Column, Cmd (Guarded.Input.Msg Int) )
updateColumn userRow inputMsg column =
    case userRow of
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
