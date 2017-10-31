module Algorism.Operands.StateTests exposing (testSuite)

import Test exposing (..)
import Expect
import Algorism.Common.Operator exposing (Operator(..))
import Algorism.Operands.Types exposing (Model, Msg(..))
import Algorism.Operands.State exposing (update, operandsOf)
import Guarded.Input
import Guarded.Input.Parsers


validOperand12 : Guarded.Input.Model Int
validOperand12 =
    Guarded.Input.initFor 12


validOperand34 : Guarded.Input.Model Int
validOperand34 =
    Guarded.Input.initFor 34


validMsg34 : Guarded.Input.Msg Int
validMsg34 =
    Guarded.Input.Parsers.intParser "34"


undefinedOperand : Guarded.Input.Model Int
undefinedOperand =
    Guarded.Input.initWith Guarded.Input.Parsers.intParser ""


undefinedMsg : Guarded.Input.Msg Int
undefinedMsg =
    Guarded.Input.Parsers.intParser ""


workInProgressOperand : Guarded.Input.Model Int
workInProgressOperand =
    Guarded.Input.initWith Guarded.Input.Parsers.intParser "-"


workInProgressMsg : Guarded.Input.Msg Int
workInProgressMsg =
    Guarded.Input.Parsers.intParser "-"


someModel : Model
someModel =
    Model validOperand12 validOperand34 Guarded.Input.Parsers.intParser Guarded.Input.Parsers.intParser Addition


testSuite : Test
testSuite =
    describe "Algorithm.Operands.State tests"
        [ describe "update tests for FirstOperandChanged messages"
            [ test "first operand -> undefined" <|
                \() ->
                    { someModel | firstOperand = undefinedOperand }
                        |> Expect.equal (update (FirstOperandChanged undefinedMsg) someModel |> Tuple.first)
            , test "first operand -> work-in-progress" <|
                \() ->
                    { someModel | firstOperand = workInProgressOperand }
                        |> Expect.equal (update (FirstOperandChanged workInProgressMsg) someModel |> Tuple.first)
            , test "first operand -> 34" <|
                \() ->
                    { someModel | firstOperand = validOperand34 }
                        |> Expect.equal (update (FirstOperandChanged validMsg34) someModel |> Tuple.first)
            ]
        , describe "update tests for SecondOperandChanged messages"
            [ test "second operand -> undefined" <|
                \() ->
                    { someModel | secondOperand = undefinedOperand }
                        |> Expect.equal (update (SecondOperandChanged undefinedMsg) someModel |> Tuple.first)
            , test "second operand -> work-in-progress" <|
                \() ->
                    { someModel | secondOperand = workInProgressOperand }
                        |> Expect.equal (update (SecondOperandChanged workInProgressMsg) someModel |> Tuple.first)
            , test "second operand -> 34" <|
                \() ->
                    { someModel | secondOperand = validOperand34 }
                        |> Expect.equal (update (SecondOperandChanged validMsg34) someModel |> Tuple.first)
            ]
        , describe "operandsOf tests"
            [ test "First operand work-in-progress yields Err" <|
                \() ->
                    Expect.true "1st op wip" ({ someModel | firstOperand = workInProgressOperand } |> operandsOf |> isErrorResult)
            , test "First operand undefined yields Err" <|
                \() ->
                    Expect.true "1st op undef" ({ someModel | firstOperand = undefinedOperand } |> operandsOf |> isErrorResult)
            , test "Second operand work-in-progress yields Err" <|
                \() ->
                    Expect.true "2nd op wip" ({ someModel | secondOperand = workInProgressOperand } |> operandsOf |> isErrorResult)
            , test "Second operand undefined yields Err" <|
                \() ->
                    Expect.true "2nd op undef" ({ someModel | secondOperand = undefinedOperand } |> operandsOf |> isErrorResult)
            , test "Both operands defined yields operand pair" <|
                \() ->
                    Ok ( 12, 34 )
                        |> Expect.equal (operandsOf someModel)
            ]
        ]


isErrorResult : Result String ( Int, Int ) -> Bool
isErrorResult result =
    case result of
        Err _ ->
            True

        _ ->
            False
