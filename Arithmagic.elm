module Arithmagic exposing (..)

import ExerciseTypes exposing (..)
import Route exposing (Route, fromLocation)
import Navigation exposing (..)
import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Html.Events exposing (onClick)
import Material
import Material.Scheme
import Material.Options as Options exposing (css)
import Material.Layout as Layout
import Material.Toggles as Toggles
import AdditionApp
import StandardSubtractionApp
import AustrianSubtractionApp


-- import Material.Button as Button
-- import Material.Color as Color
-- import Material.List as List
-- MODEL


type alias Model =
    { exercise : ExerciseType
    , additionModel : AdditionApp.Model
    , standardSubtractionModel : StandardSubtractionApp.Model
    , austrianSubtractionModel : AustrianSubtractionApp.Model
    , mdl : Material.Model
    }


model : Model
model =
    { exercise = Addition
    , additionModel = AdditionApp.initialModel
    , standardSubtractionModel = StandardSubtractionApp.initialModel
    , austrianSubtractionModel = AustrianSubtractionApp.initialModel
    , mdl = Material.model
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        exercise =
            locationToExercise location
    in
        ( { model | exercise = exercise }
        , Cmd.none
        )



-- ACTION, UPDATE


type Msg
    = SelectExercise ExerciseType
    | SelectSubtraction SubtractionType
    | UrlChanged Navigation.Location
    | AdditionMsg AdditionApp.Msg
    | StandardSubtractionMsg StandardSubtractionApp.Msg
    | AustrianSubtractionMsg AustrianSubtractionApp.Msg
    | Mdl (Material.Msg Msg)


subtractionTypeToHash : SubtractionType -> String
subtractionTypeToHash subtraction =
    if subtraction == Standard then
        "#subtraction/standard"
    else
        "#subtraction/austrian"


exerciseTypeToHash : ExerciseType -> String
exerciseTypeToHash exercise =
    case exercise of
        Addition ->
            "#addition"

        Subtraction subtraction ->
            subtractionTypeToHash subtraction


locationToExercise : Navigation.Location -> ExerciseType
locationToExercise location =
    let
        route =
            Maybe.withDefault Route.Addition <| fromLocation location
    in
        case route of
            Route.Addition ->
                Addition

            Route.StandardSubtraction ->
                Subtraction Standard

            Route.AustrianSubtraction ->
                Subtraction Austrian


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectExercise selectedExercise ->
            let
                hash =
                    exerciseTypeToHash selectedExercise
            in
                ( { model | exercise = selectedExercise }
                , Navigation.newUrl hash
                )

        SelectSubtraction selectedSubtraction ->
            let
                hash =
                    subtractionTypeToHash selectedSubtraction
            in
                ( { model | exercise = Subtraction selectedSubtraction }
                , Navigation.newUrl hash
                )

        UrlChanged location ->
            ( { model | exercise = locationToExercise location }
            , Cmd.none
            )

        AdditionMsg additionMsg ->
            let
                ( additionModel, additionCmd ) =
                    AdditionApp.update additionMsg model.additionModel
            in
                ( { model | additionModel = additionModel }
                , Cmd.map AdditionMsg additionCmd
                )

        StandardSubtractionMsg subtractionMsg ->
            let
                ( subtractionModel, subtractionCmd ) =
                    StandardSubtractionApp.update subtractionMsg model.standardSubtractionModel
            in
                ( { model | standardSubtractionModel = subtractionModel }
                , Cmd.map StandardSubtractionMsg subtractionCmd
                )

        AustrianSubtractionMsg subtractionMsg ->
            let
                ( subtractionModel, subtractionCmd ) =
                    AustrianSubtractionApp.update subtractionMsg model.austrianSubtractionModel
            in
                ( { model | austrianSubtractionModel = subtractionModel }
                , Cmd.map AustrianSubtractionMsg subtractionCmd
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
                    [ subExerciseView model
                    ]
                ]
            }


subExerciseView : Model -> Html Msg
subExerciseView model =
    case model.exercise of
        Addition ->
            Html.map AdditionMsg (AdditionApp.view model.additionModel)

        Subtraction subtractionType ->
            div
                []
                (List.append
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
                    [ subtractionView subtractionType model
                    ]
                )


subtractionView : SubtractionType -> Model -> Html Msg
subtractionView subtractionType model =
    case subtractionType of
        Standard ->
            Html.map StandardSubtractionMsg (StandardSubtractionApp.view model.standardSubtractionModel)

        Austrian ->
            Html.map AustrianSubtractionMsg (AustrianSubtractionApp.view model.austrianSubtractionModel)



-- Load Google Mdl CSS. You'll likely want to do that not in code as we
-- do here, but rather in your master .html file. See the documentation
-- for the `Material` module for details.


main : Program Never Model Msg
main =
    Navigation.program
        UrlChanged
        { init = init
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
