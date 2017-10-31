module Algorism.Subtraction.Standard.Types exposing (..)

import Guarded.Input
import Algorism.Common.Operator exposing (Operator(..))
import Algorism.Common.Util exposing (initializeForModel)


-- This implements the "Standard method"
-- described in http://web.sonoma.edu/users/w/wilsonst/courses/math_300/groupwork/altsub/standard.html


type alias Column =
    { regrouppedFirstOperand : Maybe Int
    , userRegrouppedFirstOperand : Guarded.Input.Model Int
    , borrowFromRegrouppedFirstOperand : Bool
    , userBorrowFromRegrouppedFirstOperand : Bool
    , firstOperand : Maybe Int
    , borrowFromFirstOperand : Bool
    , userBorrowFromFirstOperand : Bool
    , secondOperand : Maybe Int
    , result : Maybe Int
    , userResult : Guarded.Input.Model Int
    }


type alias Model =
    { columns : List Column
    }


type Msg
    = UserIntInputChanged UserIntInputMsg
    | UserBorrowToggled UserBoolInputMsg


type alias UserIntInputMsg =
    { userRow : IntUserRow
    , columnIndex : Int
    , inputMsg : Guarded.Input.Msg Int
    }


type IntUserRow
    = RegrouppedFirstOperand
    | Result


type alias UserBoolInputMsg =
    { userRow : BoolUserRow
    , columnIndex : Int
    }


type BoolUserRow
    = BorrowFromRegrouppedFirstOperand
    | BorrowFromFirstOperand



-- TODO rename and factor out (?)


guardedInputMsgToMsg : IntUserRow -> Int -> Guarded.Input.Msg Int -> Msg
guardedInputMsgToMsg userRow columnIndex =
    UserIntInputMsg userRow columnIndex >> UserIntInputChanged



-- TODO name is probably incorrect - check Html.Events.onDoubleClick


boolInputMsgToMsg : BoolUserRow -> Int -> Msg
boolInputMsgToMsg userRow columnIndex =
    UserBorrowToggled <| UserBoolInputMsg userRow columnIndex


initializeFor : Int -> Int -> Result String Model
initializeFor =
    initializeForModel Subtraction Model initializeColumnFor


initializeColumnFor : Maybe Int -> Maybe Int -> Column
initializeColumnFor firstOperand secondOperand =
    { regrouppedFirstOperand = Nothing
    , userRegrouppedFirstOperand = Guarded.Input.init
    , borrowFromRegrouppedFirstOperand = False
    , userBorrowFromRegrouppedFirstOperand = False
    , firstOperand = firstOperand
    , borrowFromFirstOperand = False
    , userBorrowFromFirstOperand = False
    , secondOperand = secondOperand
    , result = Nothing
    , userResult = Guarded.Input.init
    }


type alias CalculationState =
    { borrow : Bool
    , columnsDone : List Column
    }


type FirstRelativeToSecondOperand
    = FirstEqualsSecond
    | FirstGreaterThanSecond
    | FirstLessThanSecond


firstRelativeToSecondOperand : Int -> Int -> FirstRelativeToSecondOperand
firstRelativeToSecondOperand first second =
    let
        difference =
            first - second
    in
        if difference == 0 then
            FirstEqualsSecond
        else if difference < 0 then
            FirstLessThanSecond
        else
            FirstGreaterThanSecond


calculateColumn : Column -> CalculationState -> CalculationState
calculateColumn newColumn currentState =
    let
        firstOperand =
            Maybe.withDefault 0 newColumn.firstOperand

        secondOperand =
            Maybe.withDefault 0 newColumn.secondOperand

        firstRelativeToSecond =
            firstRelativeToSecondOperand firstOperand secondOperand

        ( regrouppedFirstOperand, borrowFromRegrouppedFirstOperand, firstOperandToUse, borrowFromFirstOperand, borrowFromNext ) =
            case ( currentState.borrow, firstOperand, firstRelativeToSecond ) of
                ( False, fOp, FirstLessThanSecond ) ->
                    ( Just <| firstOperand + 10, False, firstOperand + 10, False, True )

                ( False, fOp, _ ) ->
                    ( Nothing, False, firstOperand, False, False )

                ( True, 0, _ ) ->
                    ( Just 10, True, 9, False, True )

                ( True, _, FirstGreaterThanSecond ) ->
                    ( Nothing, False, firstOperand - 1, True, False )

                ( True, fOp, _ ) ->
                    ( Just <| firstOperand + 9, False, firstOperand + 9, True, True )

        result =
            firstOperandToUse - secondOperand

        maybeResult =
            Just result

        calculatedColumn =
            { newColumn
                | regrouppedFirstOperand = regrouppedFirstOperand
                , borrowFromRegrouppedFirstOperand = borrowFromRegrouppedFirstOperand
                , borrowFromFirstOperand = borrowFromFirstOperand
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
            CalculationState False []

        finalColumnsState =
            List.foldr calculateColumn seedColumnState model.columns
    in
        Model finalColumnsState.columnsDone
