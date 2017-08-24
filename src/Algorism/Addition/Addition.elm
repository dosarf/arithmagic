module Algorism.Addition.Addition exposing (..)

import Algorism.Addition.Types exposing (Column, Model, Msg(..))


initializeFor : Int -> Int -> Result String (List Column)
initializeFor firstOperand secondOperand =
    if firstOperand < 0 then
        Err <| "First operand is negative: " ++ (toString firstOperand)
    else if secondOperand < 0 then
        Err <| "Second operand is negative: " ++ (toString secondOperand)
    else
        let
            columnCount =
                1 + (max (numberOfDigits firstOperand) (numberOfDigits secondOperand))

            firstDigits =
                parseNDigits columnCount firstOperand

            secondDigits =
                parseNDigits columnCount secondOperand
        in
            Ok <| List.map2 (\x y -> initializeColumnFor x y) firstDigits secondDigits


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
    Column Nothing firstOperand secondOperand Nothing


numberOfDigits : Int -> Int
numberOfDigits integer =
    if integer < 0 then
        numberOfDigits -integer
    else if integer < 10 then
        1
    else
        1 + (numberOfDigits <| integer // 10)


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

        calculatedColumn =
            { newColumn
                | carry =
                    if carryFromPrevious == 0 then
                        Nothing
                    else
                        Just carryFromPrevious
                , result = maybeResult
            }
    in
        { carry = carryToNext
        , columnsDone = List.concat [ [ calculatedColumn ], currentState.columnsDone ]
        }


solve : List Column -> List Column
solve columns =
    let
        seedState =
            CalculationState 0 []

        finalState =
            List.foldr calculateColumn seedState columns
    in
        finalState.columnsDone
