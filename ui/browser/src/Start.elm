module Start exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy)
import Http
import Navigation
import String


main =
  Html.programWithFlags
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- MODEL


type Model
  = NoProject String
  | PotentialProject String String
  | Waiting



-- INIT


type alias Flags =
  { root : String
  , suggestion : Maybe String
  }


init : Flags -> ( Model, Cmd Msg )
init flags =
  case flags.suggestion of
    Nothing ->
      ( NoProject flags.root, Cmd.none )

    Just suggestion ->
      ( PotentialProject flags.root suggestion, Cmd.none )



-- UPDATE


type Msg
  = RequestNewProject
  | NewProjectReady
  | IgnoreSuggestion String
  | MoveReactor


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    RequestNewProject ->
      ( Waiting, createNewProject )

    NewProjectReady ->
      ( Waiting, Navigation.reload )

    IgnoreSuggestion root ->
      ( NoProject root, Cmd.none )

    MoveReactor ->
      ( Waiting, moveReactor )


createNewProject : Cmd Msg
createNewProject =
  Http.send (\_ -> NewProjectReady) <|
    Http.getString "/_elm/create-new-project"


moveReactor : Cmd Msg
moveReactor =
  Http.send (\_ -> NewProjectReady) <|
    Http.getString "/_elm/move-to-root"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  case model of
    NoProject root ->
      viewNoProject root

    PotentialProject root suggestion ->
      viewPotentialProject root suggestion

    Waiting ->
      viewWaiting



-- VIEW NO PROJECT


viewNoProject : String -> Html Msg
viewNoProject root =
  div [ class "start" ]
    [ div [ class "start-hello" ]
        [ h1 [] [ text "Hello!" ]
        , para "Looks like you are starting a new Elm project. Exciting!"
        , para "I will help you get set up. Just press this button!"
        , div [ class "button" ]
            [ a [ onClick RequestNewProject ] [ text "Create New Project" ]
            ]
        ]
    , div [ class "start-details" ]
        [ h2 [] [ text "What does this button do exactly?" ]
        , p []
            [ text "In the "
            , code [] [ text root ]
            , text " directory, I will create:"
            ]
        , ul []
            [ file "elm.json" "describes which packages you need."
            , file "src/" "a directory for all your Elm code."
            , file "src/Main.elm" "an example Elm program."
            , file "tests/" "a directory for any tests you write."
            ]
        , para "I will share more details on all this after you press the button!"
        ]
    ]


para : String -> Html msg
para str =
  p [] [ text str ]


file : String -> String -> Html msg
file fileName description =
  li []
    [ code [] [ text fileName ]
    , text " – "
    , text description
    ]



-- VIEW WAITING


viewWaiting : Html msg
viewWaiting =
  div [ class "waiting" ]
    [ img [ src "/_elm/waiting.gif" ] []
    ]



-- VIEW POTENTIAL PROJECT


viewPotentialProject : String -> String -> Html Msg
viewPotentialProject root suggestion =
  div [ class "start potential" ]
    [ h1 [] [ text "Existing Project?" ]
    , p []
        [ text "You ran "
        , code [] [ text "elm-reactor" ]
        , text " from "
        , code [] [ text root ]
        , text " which is not the root of an Elm project."
        ]
    , p []
        [ text "I found an Elm project at "
        , code [] [ text suggestion ]
        , text " though. Would you like to continue developing that one?"
        ]
    , div [ class "button" ]
        [ a [ onClick MoveReactor ] [ text "Continue Existing Project" ]
        ]
    , h1 [] [ text "Or New Project?" ]
    , para "Or maybe you want some weird nested Elm project, where you have Elm projects within other Elm projects. It is probably better to have them separated, but I can get you set up if that is what you really want!"
    , div [ class "button" ]
        [ a [ onClick (IgnoreSuggestion root) ] [ text "New Project" ]
        ]
    ]
