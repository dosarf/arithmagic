module Algorism.Addition.Types exposing (..)

import Guarded.Input


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


guardedInputMsgToMsg : UserRow -> Int -> Guarded.Input.Msg Int -> Msg
guardedInputMsgToMsg userRow columnIndex =
    UserInputMsg userRow columnIndex >> UserInputChanged


initializeFor : Int -> Int -> Result String Model
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
    { carry = Nothing
    , firstOperand = firstOperand
    , secondOperand = secondOperand
    , result = Nothing
    , userCarry = Guarded.Input.init
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


solve : Model -> Model
solve model =
    let
        seedColumnState =
            CalculationState 0 []

        finalColumnsState =
            List.foldr calculateColumn seedColumnState model.columns
    in
        Model finalColumnsState.columnsDone
