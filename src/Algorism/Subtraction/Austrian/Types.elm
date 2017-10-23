module Algorism.Subtraction.Austrian.Types exposing (..)

import Guarded.Input


-- TODO consider factoring out bits from Addition.Types.elm and this
-- TODO consider giving a better name for type UserRow (in both Addition and here)
-- This implements the "Austrian method"
-- described in http://web.sonoma.edu/users/w/wilsonst/courses/math_300/groupwork/altsub/aust.html


type alias Column =
    { borrow : Maybe Int
    , firstOperand : Maybe Int
    , secondOperand : Maybe Int
    , result : Maybe Int
    , userBorrow : Guarded.Input.Model Int
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


guardedInputMsgToMsg : UserRow -> Int -> Guarded.Input.Msg Int -> Msg
guardedInputMsgToMsg userRow columnIndex =
    UserInputMsg userRow columnIndex >> UserInputChanged



-- This is not exactly the same as Addition.initializeFor (column count is 1 less)


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
    { borrow = Nothing
    , firstOperand = firstOperand
    , secondOperand = secondOperand
    , result = Nothing
    , userBorrow = Guarded.Input.init
    , userResult = Guarded.Input.init
    }


numberOfDigits : Int -> Int
numberOfDigits integer =
    if integer < 0 then
        numberOfDigits -integer
    else if integer < 10 then
        1
    else
        1 + (numberOfDigits <| integer // 10)


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


solve : Model -> Model
solve model =
    let
        seedColumnState =
            CalculationState 0 []

        finalColumnsState =
            List.foldr calculateColumn seedColumnState model.columns
    in
        Model finalColumnsState.columnsDone
