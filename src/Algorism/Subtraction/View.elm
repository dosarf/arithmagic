module Algorism.Subtraction.View exposing (..)

import Algorism.Subtraction.Types exposing (Column, Model, Msg, guardedInputMsgToMsg, UserRow(..))
import Html exposing (Html, button, div, input, table, text, tr, td)
import Html.Attributes exposing (class, classList, value, rowspan, colspan)
import Guarded.Input
import Guarded.Input.Parsers


{-
   TODO document these CSS classes:
   algorism-subtraction-table,
   algorism-subtraction-operand-tr,
   algorism-subtraction-right-filler-td, algorism-subtraction-operand-td, algorism-subtraction-left-filler-td,

   algorism-subtraction-loan-tr, algorism-subtraction-first-operand-bottom-filler-td, algorism-subtraction-loan-td,

   algorism-subtraction-loan-td, algorism-subtraction-loan-input,

   algorism-subtraction-input-correct, algorism-subtraction-input-incorrect,
   algorism-subtraction-input-unnecessary, algorism-subtraction-input-missing,
   algorism-subtraction-input-correct-empty,


   algorism-subtraction-bottom-loan-tr, algorism-subtraction-second-operand-top-filler-td,

   algorism-subtraction-result-tr,

   algorism-subtraction-result-input, algorism-subtraction-result-td
-}


view : Model -> Html Msg
view model =
    table
        [ class "algorism-subtraction-table" ]
        [ operandTr "" model .firstOperand
        , loanTr model
        , bottomLoanTr model
        , operandTr "-" model .secondOperand
        , resultTr model
        ]


operandTr : String -> Algorism.Subtraction.Types.Model -> (Algorism.Subtraction.Types.Column -> Maybe Int) -> Html Msg
operandTr firstField addition selector =
    let
        columnCount =
            List.length addition.columns

        columnPositionFor =
            columnPosition columnCount
    in
        tr [ class "algorism-subtraction-operand-tr" ]
            ((td [] [ text firstField ])
                :: (List.concat
                        (List.indexedMap
                            (\columnIndex column -> operandTds (selector column) (columnPositionFor columnIndex))
                            addition.columns
                        )
                   )
            )


type ColumnPosition
    = MostSignificant
    | Middle
    | LeastSignificant


columnPosition : Int -> Int -> ColumnPosition
columnPosition columnCount columnIndex =
    case ( columnIndex, columnCount - 1 - columnIndex ) of
        ( 0, _ ) ->
            MostSignificant

        ( _, 0 ) ->
            LeastSignificant

        _ ->
            Middle


operandTds : Maybe Int -> ColumnPosition -> List (Html Msg)
operandTds maybeDigit columnPosition =
    case columnPosition of
        MostSignificant ->
            [ operandTd maybeDigit
            , td [ class "algorism-subtraction-right-filler-td" ] []
            ]

        Middle ->
            [ td [ class "algorism-subtraction-left-filler-td" ] []
            , operandTd maybeDigit
            , td [ class "algorism-subtraction-right-filler-td" ] []
            ]

        LeastSignificant ->
            [ td [ class "algorism-subtraction-left-filler-td" ] []
            , operandTd maybeDigit
            ]


operandTd : Maybe Int -> Html Msg
operandTd maybeDigit =
    td [ class "algorism-subtraction-operand-td" ] [ text <| maybeDigitToBlankOrString maybeDigit ]


maybeDigitToBlankOrString : Maybe Int -> String
maybeDigitToBlankOrString maybeDigit =
    Maybe.map (\digit -> toString digit) maybeDigit |> Maybe.withDefault ""


loanTr : Algorism.Subtraction.Types.Model -> Html Msg
loanTr addition =
    let
        columnCount =
            List.length addition.columns

        columnPositionFor =
            columnPosition columnCount
    in
        tr
            [ class "algorism-subtraction-loan-tr" ]
            ((td [] [])
                :: (List.concat
                        (List.indexedMap
                            (\columnIndex column -> loanTds column.loan column.userLoan columnIndex (columnPositionFor columnIndex))
                            addition.columns
                        )
                   )
            )


loanTds : Maybe Int -> Guarded.Input.Model Int -> Int -> ColumnPosition -> List (Html Msg)
loanTds maybeLoan userLoan columnIndex columnPosition =
    let
        correctnessClass =
            userInputCorrectnessClass maybeLoan userLoan
    in
        if columnPosition == LeastSignificant then
            [ td [ class "algorism-subtraction-first-operand-bottom-filler-td" ] []
            ]
        else
            [ td [ class "algorism-subtraction-first-operand-bottom-filler-td" ] []
            , td
                [ classList [ ( "algorism-subtraction-loan-td", True ), ( correctnessClass, True ) ]
                , rowspan 2
                , colspan 2
                ]
                [ input
                    [ Guarded.Input.parseOnInput (guardedInputMsgToMsg Loan columnIndex) Guarded.Input.Parsers.decimalDigitParser
                    , value <| Guarded.Input.inputString userLoan
                    , classList
                        [ ( "algorism-subtraction-loan-input", True )
                        , ( correctnessClass, True )
                        ]
                    ]
                    []
                ]
            ]


userInputCorrectnessClass : Maybe Int -> Guarded.Input.Model Int -> String
userInputCorrectnessClass maybeDigit userInput =
    let
        userInputResult =
            Guarded.Input.toResult userInput
    in
        case ( maybeDigit, userInputResult ) of
            ( Just digit2, Ok digit1 ) ->
                if digit1 == digit2 then
                    "algorism-subtraction-input-correct"
                else
                    "algorism-subtraction-input-incorrect"

            ( Nothing, Ok digit1 ) ->
                "algorism-subtraction-input-unnecessary"

            ( Just digit, Err _ ) ->
                "algorism-subtraction-input-missing"

            ( Nothing, Err _ ) ->
                "algorism-subtraction-input-correct-empty"


bottomLoanTr : Algorism.Subtraction.Types.Model -> Html Msg
bottomLoanTr addition =
    let
        columnCount =
            List.length addition.columns
    in
        tr
            [ class "algorism-subtraction-bottom-loan-tr" ]
            (List.repeat
                columnCount
                bottomLoanTd
            )


bottomLoanTd : Html Msg
bottomLoanTd =
    td [ class "algorism-subtraction-second-operand-top-filler-td" ] []


resultTr : Algorism.Subtraction.Types.Model -> Html Msg
resultTr addition =
    let
        columnCount =
            List.length addition.columns

        columnPositionFor =
            columnPosition columnCount
    in
        tr [ class "algorism-subtraction-result-tr" ]
            ((td [] [])
                :: (List.concat
                        (List.indexedMap
                            (\columnIndex column -> resultTds column.result column.userResult columnIndex (columnPositionFor columnIndex))
                            addition.columns
                        )
                   )
            )


resultTds : Maybe Int -> Guarded.Input.Model Int -> Int -> ColumnPosition -> List (Html Msg)
resultTds maybeResult userResult columnIndex columnPosition =
    let
        correctnessClass =
            userInputCorrectnessClass maybeResult userResult
    in
        case columnPosition of
            MostSignificant ->
                [ resultTd correctnessClass columnIndex maybeResult userResult
                , td [ class "algorism-subtraction-right-filler-td" ] []
                ]

            Middle ->
                [ td [ class "algorism-subtraction-left-filler-td" ] []
                , resultTd correctnessClass columnIndex maybeResult userResult
                , td [ class "algorism-subtraction-right-filler-td" ] []
                ]

            LeastSignificant ->
                [ td [ class "algorism-subtraction-left-filler-td" ] []
                , resultTd correctnessClass columnIndex maybeResult userResult
                ]


resultTd : String -> Int -> Maybe Int -> Guarded.Input.Model Int -> Html Msg
resultTd correctnessClass columnIndex maybeResult userResult =
    let
        correctnessClass =
            userInputCorrectnessClass maybeResult userResult

        -- td [ class "algorism-subtraction-result-td" ] [ text <| maybeDigitToBlankOrString maybeResult ]
    in
        td
            [ classList [ ( "algorism-subtraction-result-td", True ), ( correctnessClass, True ) ]
            ]
            [ input
                [ Guarded.Input.parseOnInput (guardedInputMsgToMsg Result columnIndex) Guarded.Input.Parsers.decimalDigitParser
                , value <| Guarded.Input.inputString userResult
                , classList
                    [ ( "algorism-subtraction-result-input", True )
                    , ( correctnessClass, True )
                    ]
                ]
                []
            ]



{-
      TODO document these CSS classes:
      algorism-subtraction-table, algorism-subtraction-static-tr, algorism-subtraction-static-td,
      algorism-subtraction-editable-tr, algorism-subtraction-editable-td,
      algorism-subtraction-loan-tr, algorism-subtraction-loan-td,
      algorism-subtraction-input, algorism-subtraction-input-correct,
      algorism-subtraction-input-incorrect, algorism-subtraction-input-unnecessary, algorism-subtraction-input-missing,
      algorism-subtraction-input-correct-empty,
      algorism-subtraction-static-filler-td,
      algorism-subtraction-loan-filler-td


   view : Model -> Html Msg
   view model =
       table
           [ class "algorism-subtraction-table" ]
           [ operandTr "" model .firstOperand
           , loanRowView model
           , operandTr "-" model .secondOperand
           , resultRowView model
           ]


   operandTr : String -> Algorism.Subtraction.Types.Model -> (Algorism.Subtraction.Types.Column -> Maybe Int) -> Html Msg
   operandTr firstField addition selector =
       tr [ class "algorism-subtraction-static-tr" ]
           (List.append
               [ td [] [ text firstField ] ]
               (List.concat
                   (List.map
                       (\column -> selector column |> createTextTds)
                       addition.columns
                   )
               )
           )


   createTextTds : Maybe Int -> List (Html Msg)
   createTextTds maybeDigit =
       [ td
           [ class "algorism-subtraction-static-td"
           ]
           [ maybeDigitToBlankOrString maybeDigit |> text ]
       , td [ class "algorism-subtraction-static-filler-td" ] []
       ]


   maybeDigitToBlankOrString : Maybe Int -> String
   maybeDigitToBlankOrString maybeDigit =
       Maybe.map (\digit -> toString digit) maybeDigit |> Maybe.withDefault ""


   resultRowView : Algorism.Subtraction.Types.Model -> Html Msg
   resultRowView subtraction =
       tr [ class "algorism-subtraction-editable-tr" ]
           (List.append
               [ td
                   [ class "algorism-subtraction-editable-td"
                   ]
                   []
               ]
               (List.concat
                   (List.indexedMap
                       (\columnIndex column -> createInputTds Result columnIndex column.result column.userResult)
                       subtraction.columns
                   )
               )
           )


   loanRowView : Algorism.Subtraction.Types.Model -> Html Msg
   loanRowView subtraction =
       tr [ class "algorism-subtraction-loan-tr" ]
           (List.append
               [ td
                   [ class "algorism-subtraction-loan-td"
                   ]
                   []
               ]
               (List.concat
                   (List.indexedMap
                       (\columnIndex column -> createInputTds Loan columnIndex column.loan column.userLoan)
                       subtraction.columns
                   )
               )
           )


   createInputTds : UserRow -> Int -> Maybe Int -> Guarded.Input.Model Int -> List (Html Msg)
   createInputTds userRow columnIndex maybeDigit userInput =
       let
           userInputResult =
               Guarded.Input.toResult userInput

           stateClass =
               case ( userInputResult, maybeDigit ) of
                   ( Ok digit1, Just digit2 ) ->
                       if digit1 == digit2 then
                           "algorism-subtraction-input-correct"
                       else
                           "algorism-subtraction-input-incorrect"

                   ( Ok digit1, Nothing ) ->
                       "algorism-subtraction-input-unnecessary"

                   ( Err _, Just digit ) ->
                       "algorism-subtraction-input-missing"

                   ( Err _, Nothing ) ->
                       "algorism-subtraction-input-correct-empty"

           tdClass =
               if userRow == Loan then
                   "algorism-subtraction-loan-td"
               else
                   "algorism-subtraction-editable-td"

           tData =
               td
                   [ classList
                       [ ( tdClass, True )
                       , ( stateClass, True )
                       ]
                   ]
                   [ input
                       [ Guarded.Input.parseOnInput (guardedInputMsgToMsg userRow columnIndex) Guarded.Input.Parsers.decimalDigitParser
                       , value <| Guarded.Input.inputString userInput
                       , classList
                           [ ( "algorism-subtraction-input", True )
                           , ( stateClass, True )
                           ]
                       ]
                       []
                   ]
       in
           case userRow of
               Result ->
                   [ tData
                   , td [ class "algorism-subtraction-static-filler-td" ] []
                   ]

               Loan ->
                   [ td [ class "algorism-subtraction-loan-filler-td" ] []
                   , tData
                   ]
-}
