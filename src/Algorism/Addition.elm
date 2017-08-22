module Algorism.Addition exposing (..)


type alias Column =
    { carry : Maybe Int
    , firstOperand : Maybe Int
    , secondOperand : Maybe Int
    , result : Maybe Int
    }


type alias Model =
    { columnCount : Int
    , columns : List Column
    }


initModel : Model
initModel =
    { columnCount = 0
    , columns = []
    }


initModelFor : Int -> Int -> Result String Model
initModelFor firstOperand secondOperand =
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
            Ok <| Model columnCount (List.map2 (\x y -> initColumn x y) firstDigits secondDigits)


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


initColumn : Maybe Int -> Maybe Int -> Column
initColumn firstOperand secondOperand =
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


calculator : Column -> CalculationState -> CalculationState
calculator newColumn currentState =
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
        seedState =
            CalculationState 0 []

        finalState =
            List.foldr calculator seedState model.columns
    in
        Model model.columnCount finalState.columnsDone



{-
   verify : Model -> Model -> List Feedback ?
   verifyUserStep : Model -> UserStep ? -> (List Feedback ?, Maybe Animation)

     - user step : in a column, writing / modifying / deleting a result / carry
     - feedback : Correc result / carry in column, or Incorrect result / carry + hint text
     - animation: carry to a column

     use cases:
      - setting correct / incorrect value at the next correct position
      - deleting a value at any position (earlier, later)
      - re-setting a value at any earlier position
      - setting a value in a later position
-}
