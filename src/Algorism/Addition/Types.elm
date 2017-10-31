module Algorism.Addition.Types exposing (..)

import Guarded.Input
import Algorism.Common.Operator exposing (Operator(..))
import Algorism.Common.Util exposing (initializeForModel)


type alias Column =
    { carry : Maybe Int
    , firstOperand : Maybe Int
    , secondOperand : Maybe Int
    , result : Maybe Int
    , userCarry : Guarded.Input.Model Int
    , userResult : Guarded.Input.Model Int
    }


type alias Model =
    { columns : List Column
    }


type Msg
    = UserInputChanged UserInputMsg


type alias UserInputMsg =
    { userRow : UserRow
    , columnIndex : Int
    , inputMsg : Guarded.Input.Msg Int
    }


type UserRow
    = Carry
    | Result



-- TODO rename and factor out (?)


guardedInputMsgToMsg : UserRow -> Int -> Guarded.Input.Msg Int -> Msg
guardedInputMsgToMsg userRow columnIndex =
    UserInputMsg userRow columnIndex >> UserInputChanged


initializeFor : Int -> Int -> Result String Model
initializeFor =
    initializeForModel Addition Model initializeColumnFor


initializeColumnFor : Maybe Int -> Maybe Int -> Column
initializeColumnFor firstOperand secondOperand =
    { carry = Nothing
    , firstOperand = firstOperand
    , secondOperand = secondOperand
    , result = Nothing
    , userCarry = Guarded.Input.init
    , userResult = Guarded.Input.init
    }


type alias CalculationState =
    { carry : Int
    , columnsDone : List Column
    }


calculateColumn : Column -> CalculationState -> CalculationState
calculateColumn newColumn currentState =
    let
        carryFromPrevious =
            currentState.carry

        firstOperand =
            Maybe.withDefault 0 newColumn.firstOperand

        secondOperand =
            Maybe.withDefault 0 newColumn.secondOperand

        sumOfCarryAndOperands =
            carryFromPrevious + firstOperand + secondOperand

        carryToNext =
            sumOfCarryAndOperands // 10

        result =
            rem sumOfCarryAndOperands 10

        -- if the operands above this result zero are both Nothing, then
        -- a zero result would merely be a padding zero - in that case
        -- emit a Nothing only
        maybeResult =
            case newColumn.firstOperand of
                Just n ->
                    Just result

                Nothing ->
                    case newColumn.secondOperand of
                        Just n ->
                            Just result

                        Nothing ->
                            if result /= 0 then
                                Just result
                            else
                                Nothing

        maybeCarry =
            if carryFromPrevious == 0 then
                Nothing
            else
                Just carryFromPrevious

        calculatedColumn =
            { newColumn
                | carry = maybeCarry
                , result = maybeResult
            }
    in
        { carry = carryToNext
        , columnsDone = List.concat [ [ calculatedColumn ], currentState.columnsDone ]
        }



-- TODO this is verbatim the same for Addition, Subtraction - can it be neatly factored out?
-- (verbatim the same still does not mean that the same 'Model' means the same type)


solve : Model -> Model
solve model =
    let
        seedColumnState =
            CalculationState 0 []

        finalColumnsState =
            List.foldr calculateColumn seedColumnState model.columns
    in
        Model finalColumnsState.columnsDone
