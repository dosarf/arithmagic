module Algorism.Addition.StateTests exposing (..)

import Test exposing (..)
import Expect
import Algorism.Addition.Types exposing (Model, Column, Msg(..))
import Algorism.Addition.State exposing (init, update)


someModel : Model
someModel =
    Model
        [ (Column Nothing Nothing Nothing Nothing)
        , (Column Nothing (Just 1) (Just 2) Nothing)
        , (Column Nothing (Just 3) (Just 4) Nothing)
        ]
        Nothing


someModel2 : Model
someModel2 =
    Model
        [ (Column Nothing Nothing Nothing Nothing)
        , (Column Nothing (Just 9) (Just 8) Nothing)
        , (Column Nothing (Just 2) (Just 4) Nothing)
        ]
        Nothing


invalidModel : Model
invalidModel =
    Model
        [ (Column Nothing Nothing Nothing Nothing)
        ]
        (Just "test failure message")


testSuite : Test
testSuite =
    describe "Algorithm.Addition.State tests"
        [ describe "Algorism.Addition.State.init tests"
            [ test "Algorism.Addition.State.init columns" <|
                \() ->
                    1
                        |> Expect.equal (List.length (init).columns)
            ]
        , describe "Algorism.Addition.State.update (message: InitializeFor) tests"
            [ test "Initializing with negative should not be OK (1)" <|
                \() ->
                    Expect.false
                        "Initializing with negative should not be OK (1)"
                        (update (InitializeFor ( -1, 123 )) init |> Tuple.first |> modelOk)
            , test "Initializing with negative should not be OK (2)" <|
                \() ->
                    Expect.false
                        "Initializing with negative should not be OK (2)"
                        (update (InitializeFor ( 321, -2 )) init |> Tuple.first |> modelOk)
            , test "Initializing with positive should be OK (3)" <|
                \() ->
                    Expect.true
                        "Initializing with positive should be OK (3)"
                        (update (InitializeFor ( 321, 231 )) init |> Tuple.first |> modelOk)
            , test "InitializeFor drops old model and produces a new one" <|
                \() ->
                    ( someModel
                    , Cmd.none
                    )
                        |> Expect.equal (update (InitializeFor ( 13, 24 )) someModel2)
            ]
        , describe "Algorism.Addition.State.update (message: Solve) tests"
            [ test "Invalid model is not solved" <|
                \() ->
                    Expect.false
                        "Solve leaves invalid model invalid"
                        (update Solve invalidModel |> Tuple.first |> modelOk)
            , test "Valid model is solved" <|
                \() ->
                    Expect.true
                        "Solve leaves valid model valid"
                        (update Solve someModel |> Tuple.first |> modelOk)
            ]
        ]


modelOk : Model -> Bool
modelOk { columns, error } =
    case error of
        Nothing ->
            True

        Just _ ->
            False
