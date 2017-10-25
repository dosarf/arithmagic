module Algorism.Subtraction.Standard.Types exposing (..)

import Guarded.Input


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


guardedInputMsgToMsg : IntUserRow -> Int -> Guarded.Input.Msg Int -> Msg
guardedInputMsgToMsg userRow columnIndex =
    UserIntInputMsg userRow columnIndex >> UserIntInputChanged



-- TODO name is probably incorrect - check Html.Events.onDoubleClick


boolInputMsgToMsg : BoolUserRow -> Int -> Msg
boolInputMsgToMsg userRow columnIndex =
    UserBorrowToggled <| UserBoolInputMsg userRow columnIndex



-- TODO This is exactly the same as Subtraction.Austrian


initializeFor : Int -> Int -> Result String Model
initializeFor firstOperand secondOperand =
    if firstOperand < 0 then
        Err <| "First operand is negative: " ++ (toString firstOperand)
    else if secondOperand < 0 then
        Err <| "Second operand is negative: " ++ (toString secondOperand)
    else if firstOperand < secondOperand then
        Err <| "First operand < second operand (" ++ (toString firstOperand) ++ "<" ++ (toString secondOperand) ++ ")"
    else
        let
            columnCount =
                max (numberOfDigits firstOperand) (numberOfDigits secondOperand)

            firstDigits =
                parseNDigits columnCount firstOperand

            secondDigits =
                parseNDigits columnCount secondOperand

            columns =
                List.map2 (\x y -> initializeColumnFor x y) firstDigits secondDigits
        in
            Ok (Model columns)



-- TODO probably identical to Algorism.Addition.Types.parseNDigits


parseNDigits : Int -> Int -> List (Maybe Int)
parseNDigits expectedLength integer =
    if expectedLength <= 0 then
        []
    else if integer < 0 then
        []
    else if integer == 0 then
        if expectedLength == 1 then
            [ Just 0 ]
        else
            List.concat [ List.repeat (expectedLength - 1) Nothing, [ Just 0 ] ]
    else
        let
            rightMostDigit =
                rem integer 10

            integerWithDigitsOnLeft =
                integer // 10

            digitsOnLeft =
                if integerWithDigitsOnLeft > 0 then
                    parseNDigits (expectedLength - 1) integerWithDigitsOnLeft
                else
                    List.repeat (expectedLength - 1) Nothing
        in
            List.concat [ digitsOnLeft, [ Just rightMostDigit ] ]


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



-- TODO probably identical to Algorism.Addition.Types.numberOfDigits


numberOfDigits : Int -> Int
numberOfDigits integer =
    if integer < 0 then
        numberOfDigits -integer
    else if integer < 10 then
        1
    else
        1 + (numberOfDigits <| integer // 10)


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


solve : Model -> Model
solve model =
    let
        seedColumnState =
            CalculationState False []

        finalColumnsState =
            List.foldr calculateColumn seedColumnState model.columns
    in
        Model finalColumnsState.columnsDone
