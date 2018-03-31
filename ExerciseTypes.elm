module ExerciseTypes exposing (..)

import List.Extra exposing (elemIndex, getAt)


-- SubtractionType


type SubtractionType
    = Standard
    | Austrian


subtractions : List SubtractionType
subtractions =
    [ Standard
    , Austrian
    ]


intToSubtraction : Int -> SubtractionType
intToSubtraction n =
    getAt n subtractions |> Maybe.withDefault Standard


subtractionToString : SubtractionType -> String
subtractionToString subtraction =
    toString subtraction



-- ExerciseType


type ExerciseType
    = Addition
    | Subtraction SubtractionType


exercises : List ExerciseType
exercises =
    [ Addition
    , Subtraction Standard
    ]


intToExercise : Int -> ExerciseType
intToExercise n =
    getAt n exercises |> Maybe.withDefault Addition


exerciseToInt : ExerciseType -> Int
exerciseToInt exercise =
    case exercise of
        Addition ->
            0

        Subtraction _ ->
            1


exerciseToString : ExerciseType -> String
exerciseToString exercise =
    exercise |> toString |> String.split " " |> List.head |> Maybe.withDefault ""


isSelectedSubtraction : ExerciseType -> Int -> Bool
isSelectedSubtraction exercise index =
    case exercise of
        Addition ->
            False

        Subtraction subtraction ->
            let
                foundIndex =
                    elemIndex subtraction subtractions |> Maybe.withDefault 0
            in
                index == foundIndex
