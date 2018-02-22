module Index exposing (main)

import Html exposing (Html)
import Json.Decode as Decode

import Index.Dashboard as Dashboard
import Index.Project as Project exposing (Project)



-- MAIN


main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  Project



-- INIT


init : Decode.Value -> (Model, Cmd msg)
init flags =
  case Project.decode flags of
    Err _ ->
      Debug.crash "TODO"

    Ok project ->
      (project, Cmd.none)



-- UPDATE


update : msg -> Model -> (Model, Cmd msg)
update _ model =
  (model, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html msg
view =
  Dashboard.view
