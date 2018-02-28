module Page.Directory exposing
  ( Info
  , File(..)
  , decoder
  )


import Json.Decode as D

import Elm.Project as Project




-- DIRECTORY


type alias Directory =
  { path : List String
  , dirs : List String
  , files : List File
  , readme : Maybe String
  }


type alias File =
  { name : String
  , runnable : Bool
  }



-- DECODER


decoder : D.Decoder Directory
decoder =
  D.map4 Directory
    (D.field "path" (D.list D.string))
    (D.field "dirs" (D.list D.string))
    (D.field "files" (D.list fileDecoder))
    (D.field "readme" (D.nullable D.string))


fileDecoder : D.Decoder File
fileDecoder =
  D.map2 File
    (D.field "name" D.string)
    (D.field "runnable" D.bool)



-- VIEW


view : Project -> Directory -> Html msg
view project directory =
  Skeleton.view
    [ Navigator.view pwd
    , section [ class "left-column" ]
        [ viewFiles directory.dirs directory.files
        , viewReadme directory.readme
        ]
    , section [ class "right-column" ]
        [ viewProjectSummary project
        , viewDeps project
        , viewTestDeps project
        ]
    , div [ style "clear" "both" ] []
    ]



-- VIEW README


viewReadme : Maybe String -> Html msg
viewReadme readme =
  case readme of
    Nothing ->
      text ""

    Just markdown ->
      Skeleton.readmeBox markdown



-- VIEW FILES


viewFiles : List String -> List File -> Html msg
viewFiles dirs files =
  Skeleton.box
    { title = "File Navigation"
    , items = List.map viewDir dirs ++ List.map viewFile files
    , footer = Nothing
    }


viewDir : String -> List (Html msg)
viewDir dir =
  [ a [ href dir ] [ Icon.folder, text dir ]
  ]


viewFile : File -> List (Html msg)
viewFile {name} =
  [ a [ href name ] [ Icon.lookup name, text name ]
  ]



-- VIEW PAGE SUMMARY


viewProjectSummary : Project.Project -> Html msg
viewProjectSummary project =
  case project of
    Project.Browser info ->
      Skeleton.box
        { title = "Source Directories"
        , items = List.map (\dir -> [text dir]) info.dirs
        , footer = Nothing
        }
        -- TODO show estimated bundle size here

    Project.Package info ->
      Skeleton.box
        { title = "Package Info"
        , items =
            [ [ text ("Name: " ++ info.name) ]
            , [ text ("Version: " ++ Version.toString info.version) ]
            , [ text ("License: " ++ License.toString info.license) ]
            ]
        , footer = Nothing
        }



-- VIEW DEPENDENCIES


viewDeps : ExactDeps -> Project.Project -> Html msg
viewDeps exactDeps project =
  let
    dependencies =
      case project of
        Project.Browser info ->
          List.map (viewDependency exactDeps) info.deps

        Project.Package info ->
          List.map (viewDependency exactDeps) info.deps
  in
  Skeleton.box
    { title = "Dependencies"
    , items = dependencies
    , footer = Nothing -- TODO Just ("/_elm/dependencies", "Add more dependencies?")
    }


viewTestDeps : ExactDeps -> Project.Project -> Html msg
viewTestDeps exactDeps project =
  let
    dependencies =
      case project of
        Project.Browser info ->
          List.map (viewDependency exactDeps) info.testDeps

        Project.Package info ->
          List.map (viewDependency exactDeps) info.testDeps
  in
  Skeleton.box
    { title = "Test Dependencies"
    , items = dependencies
    , footer = Nothing -- TODO Just ("/_elm/test-dependencies", "Add more test dependencies?")
    }


viewDependency : ExactDeps -> (Package.Name, vsn) -> List (Html msg)
viewDependency exactDeps (pkg, _) =
  case Dict.get (Package.toString pkg) exactDeps of
    Nothing ->
      [ div [ style "float" "left" ]
          [ Icon.package
          , text (Package.toString pkg)
          ]
      , div [ style "float" "right" ] [ text "???" ]
      ]

    Just version ->
      [ div [ style "float" "left" ]
          [ Icon.package
          , a [ href (toPackageUrl pkg version) ] [ text (Package.toString pkg) ]
          ]
      , div [ style "float" "right" ] [ text (Version.toString version) ]
      ]


toPackageUrl : Package.Name -> Version.Version -> String
toPackageUrl name version =
  "http://package.elm-lang.org/packages/"
  ++ Package.toString name ++ "/" ++ Version.toString version
