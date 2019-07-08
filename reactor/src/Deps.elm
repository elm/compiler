module Deps exposing (main)


import Browser
import Browser.Dom as Dom
import Dict exposing (Dict)
import Elm.Constraint as Constraint exposing (Constraint)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import Svg
import Svg.Attributes as S
import Task



-- MAIN


main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }



-- MODEL


type alias Model =
  { status : Status
  , id : Int
  -- queries
  , search : Search
  , registry : Registry
  -- history
  , past : List Change
  , future : List Change
  , origin : Origin
  }



-- STATUS


type Status
  = Failure Checkpoint (List Change)
  | Waiting Checkpoint (List Change)
  | Success Checkpoint


type alias Checkpoint =
  { direct : Dict String Bounds
  , indirect : Dict String Bounds
  }


type Bounds
  = New Version NewBounds
  | Old Version Version OldBounds


type NewBounds
  = NAny
  | NCustom Constraint


type OldBounds
  = OLocked
  | OPatch
  | OMinor
  | OMajor
  | OAny
  | OCustom Constraint



-- CHANGES


type Change
  = MassLock
  | MassPatch
  | MassMinor
  | MassMajor
  | AddDirect String
  | TweakOldDirect String OldBounds
  | TweakNewDirect String NewBounds
  | TweakOldIndirect String OldBounds
  | TweakNewIndirect String NewBounds
  | DeleteDirect String
  | DeleteIndirect String



-- PREVIEW


type alias Preview =
  { direct : Dict String PBounds
  , indirect : Dict String PBounds
  }


type PBounds
  = PNew (Maybe Version) NewBounds
  | POld Version Version OldBounds


toPreview : Origin -> Checkpoint -> List Change -> Preview
toPreview origin checkpoint changes =
  let
    toPreviewBounds _ bounds =
      case bounds of
        New vsn nb     -> PNew (Just vsn) nb
        Old old new ob -> POld old new ob

    start =
      { direct   = Dict.map toPreviewBounds checkpoint.direct
      , indirect = Dict.map toPreviewBounds checkpoint.indirect
      }
  in
  List.foldr (step origin) start changes


step : Origin -> Change -> Preview -> Preview
step origin change preview =
  case change of
    MassLock ->
      massChange OLocked preview

    MassPatch ->
      massChange OPatch preview

    MassMinor ->
      massChange OMinor preview

    MassMajor ->
      massChange OMajor preview

    AddDirect pkg ->
      let
        pBound =
          case Dict.get pkg origin.direct of
            Just vsn -> POld vsn vsn OLocked
            Nothing ->
              case Dict.get pkg origin.indirect of
                Just vsn -> POld vsn vsn OLocked
                Nothing -> PNew Nothing NAny
      in
      { direct = Dict.insert pkg pBound preview.direct
      , indirect = Dict.remove pkg preview.indirect
      }

    TweakOldDirect pkg oldBounds ->
      { direct = Dict.update pkg (alterOld oldBounds) preview.direct
      , indirect = preview.indirect
      }

    TweakNewDirect pkg newBounds ->
      { direct = Dict.update pkg (alterNew newBounds) preview.direct
      , indirect = preview.indirect
      }

    TweakOldIndirect pkg oldBounds ->
      { direct = preview.direct
      , indirect = Dict.update pkg (alterOld oldBounds) preview.indirect
      }

    TweakNewIndirect pkg newBounds ->
      { direct = preview.direct
      , indirect = Dict.update pkg (alterNew newBounds) preview.indirect
      }

    DeleteDirect pkg ->
      { direct = Dict.remove pkg preview.direct
      , indirect = preview.indirect
      }

    DeleteIndirect pkg ->
      { direct = preview.direct
      , indirect = Dict.remove pkg preview.indirect
      }


massChange : OldBounds -> Preview -> Preview
massChange oldBounds preview =
  let
    changeBounds _ bounds =
      case bounds of
        PNew vsn newBounds -> PNew vsn newBounds
        POld old new _ -> POld old new oldBounds
  in
  { direct   = Dict.map changeBounds preview.direct
  , indirect = Dict.map changeBounds preview.indirect
  }


alterOld : OldBounds -> Maybe PBounds -> Maybe PBounds
alterOld ob maybeBounds =
  case maybeBounds of
    Nothing ->
      Nothing

    Just bounds ->
      case bounds of
        PNew vsn nb    -> Just (PNew vsn nb)
        POld old new _ -> Just (POld old new ob)


alterNew : NewBounds -> Maybe PBounds -> Maybe PBounds
alterNew nb maybeBounds =
  case maybeBounds of
    Nothing ->
      Nothing

    Just bounds ->
      case bounds of
        PNew vsn _      -> Just (PNew vsn nb)
        POld old new ob -> Just (POld old new ob)



-- INIT


init : () -> (Model, Cmd Msg)
init () =
  let
    origin = startTODO
    chkp = toInitialCheckpoint origin
  in
  await chkp []
    { status = Waiting chkp []
    , id = 0
    , search = { query = "", focus = Nothing }
    , registry = registryTODO
    , past = []
    , future = []
    , origin = origin
    }


type alias Origin =
  { direct : Dict String Version
  , indirect : Dict String Version
  }


startTODO : Origin
startTODO =
  { direct =
      Dict.fromList
        [ ("elm/browser", Version 1 0 1)
        , ("elm/core", Version 1 0 2)
        , ("elm/html", Version 1 0 0)
        , ("elm/http", Version 2 0 0)
        , ("elm/json", Version 1 1 2)
        , ("elm/project-metadata-utils", Version 1 0 0)
        , ("elm/svg", Version 1 0 1)
        , ("elm-explorations/markdown", Version 1 0 0)
        ]
  , indirect =
      Dict.fromList
        [ ("elm/parser", Version 1 1 0)
        , ("elm/time", Version 1 0 0)
        , ("elm/url", Version 1 0 0)
        , ("elm/virtual-dom", Version 1 0 2)
        ]
  }



-- CHECKPOINTS


toInitialCheckpoint : Origin -> Checkpoint
toInitialCheckpoint origin =
  { direct   = Dict.map (\_ v -> Old v v OLocked) origin.direct
  , indirect = Dict.map (\_ v -> Old v v OLocked) origin.indirect
  }


toCheckpoint : Dict String Version -> Preview -> Maybe Checkpoint
toCheckpoint solution preview =
  let
    direct   = Dict.foldr (addBound solution) Dict.empty preview.direct
    indirect = Dict.foldr (addBound solution) Dict.empty preview.indirect
  in
  if Dict.size direct == Dict.size preview.direct then
    Just (Checkpoint direct indirect)
  else
    Nothing


addBound : Dict String Version -> String -> PBounds -> Dict String Bounds -> Dict String Bounds
addBound solution pkg bounds dict =
  case Dict.get pkg solution of
    Nothing ->
      dict

    Just new ->
      case bounds of
        PNew _ newBounds ->
          Dict.insert pkg (New new newBounds) dict

        POld old _ oldBounds ->
          Dict.insert pkg (Old old new oldBounds) dict



-- UPDATE


type Msg
  = NoOp
  | Commit Change
  | Undo
  | Redo
  | GotSolution Int (Result Http.Error (Dict String Version))
  | SearchTouched SearchMsg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case Debug.log "msg" msg of
    NoOp ->
      ( model, Cmd.none )

    Commit latest ->
      let (checkpoint, changes) = getCheckpoint model.status in
      await checkpoint (latest::changes) { model | future = [] }

    Undo ->
      case getCheckpoint model.status of
        (checkpoint, latest :: previous) ->
          await checkpoint previous { model | future = latest :: model.future }

        (_, []) ->
          case model.past of
            [] -> ( model, Cmd.none )

            latest :: previous ->
              await (toInitialCheckpoint model.origin) previous
                { model | past = [], future = latest :: model.future }

    Redo ->
      case model.future of
        [] ->
          ( model, Cmd.none )

        next :: nexterer ->
          let (checkpoint, changes) = getCheckpoint model.status in
          await checkpoint (next::changes) { model | future = nexterer }

    GotSolution id result ->
      if model.id /= id then
        ( model, Cmd.none )
      else
        let
          (oldCheckpoint, changes) = getCheckpoint model.status
        in
        case result of
          Err _ ->
            ( { model | status = Failure oldCheckpoint changes }, Cmd.none )

          Ok solution ->
            case toCheckpoint solution (toPreview model.origin oldCheckpoint changes) of
              Nothing ->
                ( { model | status = Failure oldCheckpoint changes }
                , Cmd.none
                )

              Just newCheckpoint ->
                ( { model
                      | status = Success newCheckpoint
                      , past = changes ++ model.past
                  }
                , Cmd.none
                )

    SearchTouched searchMsg ->
      case updateSearch model.registry searchMsg model.search of
        SNone ->
          ( model, Cmd.none )

        SUpdate newSearch ->
          ( { model | search = newSearch }
          , Cmd.none
          )

        SManualBlur newSearch ->
          ( { model | search = newSearch }
          , Task.attempt (\_ -> NoOp) (Dom.blur searchDepsID)
          )

        SAdd name ->
          let (checkpoint, changes) = getCheckpoint model.status in
          await checkpoint (AddDirect name :: changes)
            { model
                | search = { query = "", focus = Nothing }
                , future = []
            }


getCheckpoint : Status -> (Checkpoint, List Change)
getCheckpoint status =
  case status of
    Failure chkp cs -> (chkp, cs)
    Waiting chkp cs -> (chkp, cs)
    Success chkp    -> (chkp, [])


await : Checkpoint -> List Change -> Model -> (Model, Cmd Msg)
await checkpoint changes model =
  let
    id = model.id + 1
    preview = toPreview model.origin checkpoint changes
  in
  (
    { model
        | status = Waiting checkpoint changes
        , id = id
    }
  ,
    Http.post
      { url = "/elm-stuff/solve"
      , body =
          Http.jsonBody <|
            E.object
              [ ("direct", E.dict identity encodeConstraint preview.direct)
              , ("indirect", E.dict identity encodeConstraint preview.indirect)
              ]
      , expect = Http.expectJson (GotSolution id) solutionDecoder
      }
  )



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "elm.json"
  , body =
      [ span
          [ style "width" "calc(100% - 500px - 2em)"
          , style "position" "fixed"
          , style "top" "0"
          , style "left" "0"
          , style "bottom" "0"
          , style "overflow-x" "hidden"
          , style "overflow-y" "scroll"
          , style "filter" "blur(4px)"
          , style "white-space" "pre"
          , style "font-family" "monospace"
          ]
          [ text elmJson
          ]
      , viewEditPanel model
      ]
  }


viewEditPanel : Model -> Html Msg
viewEditPanel model =
  div
    [ style "width" "500px"
    , style "position" "fixed"
    , style "top" "0"
    , style "right" "0"
    , style "bottom" "0"
    , style "overflow-y" "scroll"
    , style "background-color" "white"
    , style "padding" "1em"
    ]
    [ node "style" [] [ text styles ]
    , div
        [ style "display" "flex"
        , style "justify-content" "space-between"
        ]
        [ viewMassUpdates
        , lazy3 viewUndoRedo model.status model.past model.future
        ]
    , div
        [ style "display" "flex"
        , style "justify-content" "space-between"
        , style "align-items" "center"
        ]
        [ h2 [] [ text "Dependencies" ]
        , Html.map SearchTouched <|
            lazy4 viewSearch searchDepsID "Package Search" model.registry model.search
        ]
    , lazy2 viewStatus model.origin model.status
    ]


viewMassUpdates : Html Msg
viewMassUpdates =
  div []
    [ text "Mass Updates: "
    , activeButton (Commit MassLock ) (text "LOCK")
    , activeButton (Commit MassPatch) (text "PATCH")
    , activeButton (Commit MassMinor) (text "MINOR")
    , activeButton (Commit MassMajor) (text "MAJOR")
    ]


viewUndoRedo : Status -> List Change -> List Change -> Html Msg
viewUndoRedo status past future =
  let
    hasNoPast =
      List.isEmpty past &&
      case status of
        Failure _ cs -> List.isEmpty cs
        Waiting _ cs -> List.isEmpty cs
        Success _    -> True

    hasNoFuture =
      List.isEmpty future
  in
  div []
    [ if hasNoPast   then inactiveButton undoIcon else activeButton Undo undoIcon
    , if hasNoFuture then inactiveButton redoIcon else activeButton Redo redoIcon
    ]


activeButton : msg -> Html msg -> Html msg
activeButton msg content =
  button [ class "button", onClick msg ] [ content ]


inactiveButton : Html msg -> Html msg
inactiveButton content =
  button [ class "button-inactive" ] [ content ]



-- VIEW STATUS


viewStatus : Origin -> Status -> Html Msg
viewStatus origin status =
  let
    (directs, indirects) = viewStatusRows origin status
  in
  div []
    [ viewTable "Direct"   <| Dict.toList directs
    , viewTable "Indirect" <| Dict.toList indirects
    ]


viewStatusRows : Origin -> Status -> (Dict String (Html Msg), Dict String (Html Msg))
viewStatusRows origin status =
  case status of
    Failure checkpoint changes ->
      let preview = toPreview origin checkpoint changes in
      ( Dict.map (lazy2 viewWaitingRow) preview.direct
      , Dict.map (lazy2 viewWaitingRow) preview.indirect
      )

    Waiting checkpoint changes ->
      let preview = toPreview origin checkpoint changes in
      ( Dict.map (lazy2 viewWaitingRow) preview.direct
      , Dict.map (lazy2 viewWaitingRow) preview.indirect
      )

    Success checkpoint ->
      ( Dict.map (lazy2 viewSuccessRow) checkpoint.direct
      , Dict.map (lazy2 viewSuccessRow) checkpoint.indirect
      )


viewSuccessRow : String -> Bounds -> Html Msg
viewSuccessRow pkg bounds =
  case bounds of
    New version newBounds ->
      viewRow pkg (RowNew version)

    Old old new oldBounds ->
      viewRow pkg (RowOld old new)


viewWaitingRow : String -> PBounds -> Html Msg
viewWaitingRow pkg bounds =
  case bounds of
    PNew vsn newBounds ->
      viewRow pkg (RowNewGuess vsn)

    POld old new oldBounds ->
      viewRow pkg (RowOldGuess old new)



-- VIEW TABLE


viewTable : String -> List (String, Html Msg) -> Html Msg
viewTable title rows =
  table [ style "padding-bottom" "1em" ]
    [ viewColgroup
    , thead [] [ tr [] [ td [ class "table-title" ] [ text title ] ] ]
    , Keyed.node "tbody" [] rows
    ]


viewColgroup : Html msg
viewColgroup =
  colgroup []
    [ col [ style "width" "350px" ] []
    , col [ style "width" "50px" ] []
    , col [ style "width" "50px" ] []
    , col [ style "width" "50px" ] []
    ]


type RowInfo
  = RowNew Version
  | RowOld Version Version
  | RowNewGuess (Maybe Version)
  | RowOldGuess Version Version


viewRow : String -> RowInfo -> Html msg
viewRow pkg info =
  case info of
    RowNew vsn ->
      viewRowHelp pkg (text "") (text "") (viewVersion "black" vsn)

    RowNewGuess Nothing ->
      viewRowHelp pkg (text "") (text "") (text "")

    RowNewGuess (Just v) ->
      viewRowHelp pkg (text "") (text "") (viewVersion "#eeeeee" v)

    RowOld old new ->
      if old == new
      then viewRowHelp pkg (text "") (text "") (viewVersion "#cccccc" new)
      else viewRowHelp pkg (viewVersion "#cccccc" old) (viewArrow "#cccccc") (viewVersion "black" new)

    RowOldGuess old new  ->
      if old == new
      then viewRowHelp pkg (text "") (text "") (viewVersion "#eeeeee" new)
      else viewRowHelp pkg (viewVersion "#eeeeee" old) (viewArrow "#eeeeee") (viewVersion "#eeeeee" new)


viewRowHelp : String -> Html msg -> Html msg -> Html msg -> Html msg
viewRowHelp pkg oldHtml arrowHtml newHtml =
  tr []
    [ td [ style "font-family" "monospace" ] [ text pkg ]
    , td [ style "text-align" "right"      ] [ oldHtml ]
    , td [ style "text-align" "center"     ] [ arrowHtml ]
    , td [                                 ] [ newHtml ]
    ]


viewVersion : String -> Version -> Html msg
viewVersion color (Version x y z) =
  span
    [ style "font-family" "monospace"
    , style "color" color
    , style "transition" "color 1s"
    ]
    [ text (v2s x y z)
    ]


viewArrow : String -> Html msg
viewArrow color =
  span
    [ style "color" color
    , style "transition" "color 1s"
    ]
    [ text "→"
    ]



-- REGISTRY


type alias Registry = Dict String (List Char)


toRegistry : List String -> Registry
toRegistry packages =
  Dict.fromList (List.map (\n -> (n, toSearchChars n)) packages)


toSearchChars : String -> List Char
toSearchChars string =
  String.toList (String.toLower string)


registryTODO : Registry
registryTODO =
  toRegistry
    [ "elm-explorations/test"
    , "elm-explorations/markdown"
    , "elm/browser"
    , "elm/bytes"
    , "elm/core"
    , "elm/file"
    , "elm/html"
    , "elm/http"
    , "elm/json"
    , "elm/project-metadata-utils"
    , "elm/svg"
    , "elm/parser"
    , "elm/time"
    , "elm/url"
    , "elm/virtual-dom"
    ]



-- SEARCH


type alias Search =
  { query : String
  , focus : Maybe Int
  }


type SearchMsg
  = SChanged String
  | SUp
  | SDown
  | SFocus
  | SBlur
  | SEscape
  | SEnter
  | SClickAdd
  | SClickMatch String


type SearchNext
  = SNone
  | SUpdate Search
  | SManualBlur Search
  | SAdd String


updateSearch : Registry -> SearchMsg -> Search -> SearchNext
updateSearch registry msg search =
  case msg of
    SChanged query ->
      SUpdate { query = query, focus = Just 0 }

    SUp ->
      let
        newFocus = Maybe.map (\n -> Basics.max 0 (n - 1)) search.focus
      in
      SUpdate { search | focus = newFocus }

    SDown ->
      let
        numMatches = List.length (getBestMatches search.query registry)
        newFocus = Maybe.map (\n -> Basics.min numMatches (n + 1)) search.focus
      in
      SUpdate { search | focus = newFocus }

    SFocus ->
      SUpdate { search | focus = Just 0 }

    SBlur ->
      SUpdate { search | focus = Nothing }

    SEscape ->
      SManualBlur { search | focus = Nothing }

    SEnter ->
      case search.focus of
        Nothing ->
          SNone

        Just 0 ->
          if Dict.member search.query registry
          then SAdd search.query
          else SNone

        Just n ->
          case getMatch n (getBestMatches search.query registry) of
            Just match -> SUpdate { query = match, focus = Just 0 }
            Nothing -> SNone

    SClickAdd ->
      if Dict.member search.query registry
      then SAdd search.query
      else SNone

    SClickMatch match ->
      SUpdate { query = match, focus = Just 0 }


getMatch : Int -> List (Int, String) -> Maybe String
getMatch n matches =
  case matches of
    [] ->
      Nothing

    (_, match) :: worseMatches ->
      if n <= 0 then
        Nothing
      else if n == 1 then
        Just match
      else
        getMatch (n-1) worseMatches



-- VIEW SEARCH


searchDepsID : String
searchDepsID = "search-deps"


searchTestID : String
searchTestID = "search-test"


viewSearch : String -> String -> Registry -> Search -> Html SearchMsg
viewSearch searchID ghostText registry search =
  div [ style "position" "relative" ]
    [ lazy3 viewSearchQuery searchID ghostText search.query
    , lazy2 viewSearchAdd search.query registry
    , lazy3 viewSearchMatches search.query search.focus registry
    ]


viewSearchAdd : String -> Registry -> Html SearchMsg
viewSearchAdd query registry =
  if Dict.member query registry then
    activeButton SClickAdd (text "Add")
  else
    inactiveButton (text "Add")


viewSearchMatches : String -> Maybe Int -> Registry -> Html SearchMsg
viewSearchMatches query focus registry =
  case focus of
    Nothing ->
      text ""

    Just n ->
      if String.isEmpty query
      then text ""
      else
        case getBestMatches query registry of
          [] ->
            text ""

          bestMatches ->
            div [ class "search-matches" ] <|
              List.indexedMap (viewSearchMatch (n-1)) bestMatches


viewSearchMatch : Int -> Int -> (Int, String) -> Html SearchMsg
viewSearchMatch target actual (_, name) =
  div
    [ class "search-match"
    , classList [("search-match-focused", target == actual)]
    , onClick (SClickMatch name)
    ]
    [ div [ style "padding" "0.5em 1em" ] [ text name ]
    ]



-- VIEW SEARCH QUERY


viewSearchQuery : String -> String -> String -> Html SearchMsg
viewSearchQuery searchID ghostText query =
  input
    [ type_ "text"
    , id searchID
    , placeholder ghostText
    , autocomplete False
    , class "search-input"
    , value query
    , onInput SChanged
    , on "keydown" keyDecoder
    , onFocus SFocus
    , onBlur SBlur
    ]
    []


keyDecoder : D.Decoder SearchMsg
keyDecoder =
  let
    check up down enter escape value =
      if value == up then
        D.succeed SUp
      else if value == down then
        D.succeed SDown
      else if value == enter then
        D.succeed SEnter
      else if value == escape then
        D.succeed SEscape
      else
        D.fail "not up or down"
  in
  D.oneOf
    [ D.field "key" D.string
        |> D.andThen (check "ArrowUp" "ArrowDown" "Enter" "Escape")
    , D.field "keyCode" D.int
        |> D.andThen (check 38 40 13 27)
    ]



-- MATCHES


getBestMatches : String -> Registry -> List (Int, String)
getBestMatches query registry =
  Dict.foldl (addMatch (toSearchChars query)) [] registry


addMatch : List Char -> String -> List Char -> List (Int, String) -> List (Int, String)
addMatch queryChars targetName targetChars bestMatches =
  case distance 0 queryChars targetChars of
    Nothing ->
      bestMatches

    Just dist ->
      insert 4 targetName dist bestMatches


insert : Int -> String -> Int -> List (Int, String) -> List (Int, String)
insert limit name dist bestMatches =
  if limit <= 0 then
    bestMatches
  else
    case bestMatches of
      [] ->
        [ (dist, name) ]

      ((bestDist, bestName) as best) :: worseMatches ->
        if dist < bestDist then
          (dist, name) :: List.take (limit - 1) bestMatches
        else
          best :: insert (limit - 1) name dist worseMatches


distance : Int -> List Char -> List Char -> Maybe Int
distance dist queryChars targetChars =
  case queryChars of
    [] ->
      case dist + List.length targetChars of
        0 -> Nothing
        n -> Just n

    qc :: qcs ->
      case targetChars of
        [] ->
          Nothing

        tc :: tcs ->
          if qc == tc then
            distance dist qcs tcs
          else
            distance (dist + 1) queryChars tcs



-- ICONS


undoIcon : Html msg
undoIcon =
  icon "M255.545 8c-66.269.119-126.438 26.233-170.86 68.685L48.971 40.971C33.851 25.851 8 36.559 8 57.941V192c0 13.255 10.745 24 24 24h134.059c21.382 0 32.09-25.851 16.971-40.971l-41.75-41.75c30.864-28.899 70.801-44.907 113.23-45.273 92.398-.798 170.283 73.977 169.484 169.442C423.236 348.009 349.816 424 256 424c-41.127 0-79.997-14.678-110.63-41.556-4.743-4.161-11.906-3.908-16.368.553L89.34 422.659c-4.872 4.872-4.631 12.815.482 17.433C133.798 479.813 192.074 504 256 504c136.966 0 247.999-111.033 248-247.998C504.001 119.193 392.354 7.755 255.545 8z"


redoIcon : Html msg
redoIcon =
  icon "M256.455 8c66.269.119 126.437 26.233 170.859 68.685l35.715-35.715C478.149 25.851 504 36.559 504 57.941V192c0 13.255-10.745 24-24 24H345.941c-21.382 0-32.09-25.851-16.971-40.971l41.75-41.75c-30.864-28.899-70.801-44.907-113.23-45.273-92.398-.798-170.283 73.977-169.484 169.442C88.764 348.009 162.184 424 256 424c41.127 0 79.997-14.678 110.629-41.556 4.743-4.161 11.906-3.908 16.368.553l39.662 39.662c4.872 4.872 4.631 12.815-.482 17.433C378.202 479.813 319.926 504 256 504 119.034 504 8.001 392.967 8 256.002 7.999 119.193 119.646 7.755 256.455 8z"


unlockIcon : Html msg
unlockIcon =
  icon "M423.5 0C339.5.3 272 69.5 272 153.5V224H48c-26.5 0-48 21.5-48 48v192c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V272c0-26.5-21.5-48-48-48h-48v-71.1c0-39.6 31.7-72.5 71.3-72.9 40-.4 72.7 32.1 72.7 72v80c0 13.3 10.7 24 24 24h32c13.3 0 24-10.7 24-24v-80C576 68 507.5-.3 423.5 0z"


lockIcon : Html msg
lockIcon =
  icon "M400 224h-24v-72C376 68.2 307.8 0 224 0S72 68.2 72 152v72H48c-26.5 0-48 21.5-48 48v192c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V272c0-26.5-21.5-48-48-48zm-104 0H152v-72c0-39.7 32.3-72 72-72s72 32.3 72 72v72z"


icon : String -> Html msg
icon path =
  div
    [ style "display" "inline-flex"
    , style "align-self" "center"
    , style "top" ".125em"
    , style "position" "relative"
    ]
    [ Svg.svg
        [ S.viewBox "0 0 512 512"
        , S.width "1em"
        , S.height "1em"
        ]
        [ Svg.path
            [ S.fill "currentColor"
            , S.d path
            ]
            []
        ]
    ]



-- VERSIONS


type Version =
  Version Int Int Int



-- ENCODE CONSTRAINTS


encodeConstraint : PBounds -> E.Value
encodeConstraint bounds =
  case bounds of
    POld (Version x y z) _ oldBounds ->
      case oldBounds of
        OLocked   -> E.string <| v2s x y z ++ " <= v < " ++ v2s x y (z + 1)
        OPatch    -> E.string <| v2s x y z ++ " <= v < " ++ v2s x y max16
        OMinor    -> E.string <| v2s x y z ++ " <= v < " ++ v2s x max16 0
        OMajor    -> E.string <| v2s x y z ++ " <= v < " ++ v2s max16 0 0
        OAny      -> encodeAny
        OCustom c -> Constraint.encode c

    PNew _ newBounds ->
      case newBounds of
        NAny      -> encodeAny
        NCustom c -> Constraint.encode c


encodeAny : E.Value
encodeAny =
  E.string <| v2s 1 0 0 ++ " <= v <= " ++ v2s max16 max16 max16


max16 : Int
max16 =
  65535


v2s : Int -> Int -> Int -> String
v2s major minor patch =
  String.fromInt major ++ "." ++ String.fromInt minor ++ "." ++ String.fromInt patch



-- DECODE SOLUTION


solutionDecoder : D.Decoder (Dict String Version)
solutionDecoder =
  D.dict versionDecoder


versionDecoder : D.Decoder Version
versionDecoder =
  let
    toVersion str =
      case fromString str of
        Just vsn -> D.succeed vsn
        Nothing -> D.fail "invalid version number"
  in
  D.andThen toVersion D.string


fromString : String -> Maybe Version
fromString string =
  case List.map String.toInt (String.split "." string) of
    [Just major, Just minor, Just patch] ->
      fromStringHelp major minor patch

    _ ->
      Nothing


fromStringHelp : Int -> Int -> Int -> Maybe Version
fromStringHelp major minor patch =
  if major >= 0 && minor >= 0 && patch >= 0 then
    Just (Version major minor patch)
  else
    Nothing



-- TODO delete everything below here


styles : String
styles = """
body {
  font-family: sans-serif;
  font-size: 16px;
  background-color: #cccccc;
}
.search-input {
  padding: 0.5em 1em;
  border: 1px solid #cccccc;
  border-radius: 2px;
}
.search-matches {
  position: absolute;
  top: 100%;
  left: 0;
  right: 0;
  background-color: white;
}
.search-match {
  border-left: 1px solid #cccccc;
  border-right: 1px solid #cccccc;
  border-bottom: 1px solid #cccccc;
}
.search-match:hover {
  background-color: #eeeeee;
  cursor: pointer;
}
.search-match-focused {
  background-color: #60B5CC !important;
  border-color: #60B5CC;
  color: white;
}
.button {
  padding: 0.5em 1em;
  border: 1px solid #60B5CC;
  background-color: white;
  border-radius: 2px;
  color: #60B5CC;
}
.button:hover {
  color: white;
  background-color: #60B5CC;
}
.button:active {
  color: white;
  border-color: #5A6378;
  background-color: #5A6378;
}
.button-inactive {
  padding: 0.5em 1em;
  border: 1px solid #cccccc;
  background-color: white;
  border-radius: 2px;
  color: #cccccc;
}
.table-title {
  text-transform: uppercase;
  color: #cccccc;
  font-size: .75em;
}
"""


elmJson : String
elmJson = """
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.0",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.1",
            "elm/core": "1.0.2",
            "elm/html": "1.0.0",
            "elm/http": "2.0.0",
            "elm/json": "1.1.2",
            "elm/project-metadata-utils": "1.0.0",
            "elm/svg": "1.0.1",
            "elm-explorations/markdown": "1.0.0"
        },
        "indirect": {
            "elm/bytes": "1.0.7",
            "elm/file": "1.0.1",
            "elm/parser": "1.1.0",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.2"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
"""
