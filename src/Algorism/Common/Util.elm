module Algorism.Common.Util exposing (..)

import Algorism.Common.Operator exposing (Operator(..))


{- Calculates the number of digits in an integer (ignoring any negative sign). -}


numberOfDigits : Int -> Int
numberOfDigits integer =
    if integer < 0 then
        numberOfDigits -integer
    else if integer < 10 then
        1
    else
        1 + (numberOfDigits <| integer // 10)



{- Parses an integer into a list of decimal digits of at most of an expected
   length.
-}


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


initializeForModel : Operator -> (List column -> model) -> (Maybe Int -> Maybe Int -> column) -> Int -> Int -> Result String model
initializeForModel operator model column firstOperand secondOperand =
    if firstOperand < 0 then
        Err <| "First operand is negative: " ++ (toString firstOperand)
    else if secondOperand < 0 then
        Err <| "Second operand is negative: " ++ (toString secondOperand)
    else if operator == Subtraction && firstOperand < secondOperand then
        Err <| "First operand < second operand (" ++ (toString firstOperand) ++ "<" ++ (toString secondOperand) ++ ")"
    else
        let
            extraColumnCount =
                case operator of
                    Addition ->
                        1

                    Subtraction ->
                        0

            columnCount =
                extraColumnCount + max (numberOfDigits firstOperand) (numberOfDigits secondOperand)

            firstDigits =
                parseNDigits columnCount firstOperand

            secondDigits =
                parseNDigits columnCount secondOperand

            columns =
                List.map2 (\x y -> column x y) firstDigits secondDigits
        in
            Ok (model columns)
