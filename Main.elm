module Main exposing (..)

import Html exposing (Html, text)


type alias Model =
    { stuff : Int
    }


initialModel : Model
initialModel =
    { stuff = 0
    }


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    "Hello "
        ++ (toString model.stuff)
        |> text


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
