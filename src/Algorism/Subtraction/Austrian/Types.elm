module Algorism.Subtraction.Austrian.Types exposing (..)

import Guarded.Input
import Algorism.Common.Operator exposing (Operator(..))
import Algorism.Common.Util exposing (initializeForModel)


-- TODO consider factoring out bits from Addition.Types.elm and this
-- TODO consider giving a better name for type UserRow (in both Addition and here)
-- This implements the "Austrian method"
-- described in http://web.sonoma.edu/users/w/wilsonst/courses/math_300/groupwork/altsub/aust.html


type alias Column =
    { borrow : Maybe Int
    , userBorrow : Guarded.Input.Model Int
    , firstOperand : Maybe Int
    , secondOperand : Maybe Int
    , result : Maybe Int
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
    = Borrow
    | Result



-- TODO rename and factor out (?)


guardedInputMsgToMsg : UserRow -> Int -> Guarded.Input.Msg Int -> Msg
guardedInputMsgToMsg userRow columnIndex =
    UserInputMsg userRow columnIndex >> UserInputChanged


initializeFor : Int -> Int -> Result String Model
initializeFor =
    initializeForModel Subtraction Model initializeColumnFor


initializeColumnFor : Maybe Int -> Maybe Int -> Column
initializeColumnFor firstOperand secondOperand =
    { borrow = Nothing
    , userBorrow = Guarded.Input.init
    , firstOperand = firstOperand
    , secondOperand = secondOperand
    , result = Nothing
    , userResult = Guarded.Input.init
    }


type alias CalculationState =
    { borrow : Int
    , columnsDone : List Column
    }


calculateColumn : Column -> CalculationState -> CalculationState
calculateColumn newColumn currentState =
    let
        firstOperandDigit =
            Maybe.withDefault 0 newColumn.firstOperand

        secondOperand =
            (Maybe.withDefault 0 newColumn.secondOperand) + currentState.borrow

        ( borrowFromNext, firstOperand ) =
            if firstOperandDigit < secondOperand then
                ( 1, firstOperandDigit + 10 )
            else
                ( 0, firstOperandDigit )

        result =
            firstOperand - secondOperand

        maybeResult =
            Just result

        maybeBorrow =
            if currentState.borrow == 0 then
                Nothing
            else
                Just currentState.borrow

        calculatedColumn =
            { newColumn
                | borrow = maybeBorrow
                , result = maybeResult
            }
    in
        { borrow = borrowFromNext
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
