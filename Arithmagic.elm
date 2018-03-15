module Arithmagic exposing (..)

import ExerciseTypes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Html.Events exposing (onClick)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Layout as Layout
import Material.Color as Color
import Material.List as List
import Material.Toggles as Toggles
import Debug exposing (log)


-- MODEL


type alias Model =
    { exercise : ExerciseType
    , mdl : Material.Model
    }


model : Model
model =
    { exercise = Addition
    , mdl = Material.model
    }



-- ACTION, UPDATE


type Msg
    = SelectExercise ExerciseType
    | SelectSubtraction SubtractionType
    | Mdl (Material.Msg Msg)



-- Boilerplate: Msg clause for internal Mdl messages.


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectExercise selectedExercise ->
            ( { model | exercise = selectedExercise }
            , Cmd.none
            )

        SelectSubtraction selectedSubtraction ->
            ( { model | exercise = Subtraction selectedSubtraction }
            , Cmd.none
            )

        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model



-- VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    Material.Scheme.top <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            , Layout.fixedDrawer
            , Layout.selectedTab (exerciseToInt model.exercise)
            , Layout.onSelectTab (intToExercise >> SelectExercise)
            ]
            { header = [ h3 [ style [ ( "padding", "1rem" ) ] ] [ text "Arithmagic" ] ]
            , drawer = []
            , tabs =
                ( List.map (\exercise -> exerciseToString exercise |> text) exercises
                , []
                )
            , main =
                [ div
                    [ style [ ( "padding", "1rem" ) ] ]
                    [ div
                        []
                        [ text ("Exercise: " ++ (exerciseToString model.exercise)) ]
                    , subExerciseView model
                    ]
                ]
            }


subExerciseView : Model -> Html Msg
subExerciseView model =
    case model.exercise of
        Addition ->
            div [] []

        Subtraction _ ->
            div
                []
                (List.indexedMap
                    (\index subtraction ->
                        div
                            []
                            [ Toggles.radio Mdl
                                [ 1, index ]
                                model.mdl
                                [ Toggles.value (isSelectedSubtraction model.exercise index)
                                , Toggles.group "SubtractionRadioGroup"
                                , Toggles.ripple
                                , Options.onToggle (intToSubtraction index |> SelectSubtraction)
                                ]
                                [ text <| subtractionToString subtraction ]
                            ]
                    )
                    subtractions
                )



-- Load Google Mdl CSS. You'll likely want to do that not in code as we
-- do here, but rather in your master .html file. See the documentation
-- for the `Material` module for details.


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
