module Index.Dashboard exposing
  ( view
  )


import Html exposing (..)
import Html.Attributes exposing (class, href, style, title)

import Elm.Config as Config exposing (Config)
import Elm.License as License
import Elm.Repo as Repo exposing (Repo)
import Elm.Version as Version exposing (Version)
import Index.Icon as Icon
import Index.Navigator as Navigator exposing ((</>))
import Index.Project as Project exposing (Project, File(..))
import Index.Skeleton as Skeleton



-- VIEW


view : Project -> Html msg
view { root, pwd, dirs, files, readme, config } =
  Skeleton.view
    [ Navigator.view root pwd
    , viewLeftColumn dirs files readme
    , viewRightColumn config
    , clearfix
    ]


clearfix : Html msg
clearfix =
  div [ style "clear" "both" ] []



-- VIEW LEFT COLUMN


viewLeftColumn : List String -> List File -> Maybe String -> Html msg
viewLeftColumn dirs files readme =
  section [ class "left-column" ]
    [ viewFiles dirs files
    , viewReadme readme
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
viewFile file =
  case file of
    HasMain name ->
      [ a [ href name ] [ Icon.lookup name, text name ]
      ]

    NoMain name ->
      [ a [ href name ] [ Icon.lookup name, text name ]
      ]

    Other name ->
      [ a [ href name ] [ Icon.lookup name, text name ]
      ]



-- VIEW RIGHT COLUMN


viewRightColumn : Config -> Html msg
viewRightColumn config =
  div [ class "right-column" ] <|
    case config of
      Config.App info ->
        [ viewPageSummary info.pages
        , viewDeps info.dependencies
        , viewTestDeps info.testDependencies
        ]

      Config.Pkg info ->
        [ viewPackageSummary info
        , viewDeps (exactify info.dependencies)
        , viewTestDeps (exactify info.testDependencies)
        ]



-- VIEW PAGE SUMMARY


viewPageSummary : List String -> Html msg
viewPageSummary pages =
  Skeleton.box
    { title = "Pages"
    , items = List.map viewPage pages
    , footer = Nothing
    }


viewPage : String -> List (Html msg)
viewPage name =
  [ div
      [ style "float" "left"
      ]
      [ Icon.gift
      , text name
      ]
  , div
      [ style "float" "right"
      ]
      [ text "7kb"
      , bundleSize 10
      ]
  ]


bundleSize : Int -> Html msg
bundleSize size =
  span
    [ style "color" "#e1e1e1"
    ]
    [ text (" + " ++ String.fromInt size ++ "kb")
    ]



-- VIEW PACKAGE SUMMARY


viewPackageSummary : Config.PkgInfo -> Html msg
viewPackageSummary { summary, license, version } =
  Skeleton.box
    { title = "Package Information"
    , items =
        [ [ text ("Version: " ++ Version.toString version) ]
        , [ text ("License: " ++ license.code) ]
        ]
    , footer = Nothing
    }



-- VIEW DEPENDENCIES


viewDeps : Config.Deps Version -> Html msg
viewDeps deps =
  Skeleton.box
    { title = "Dependencies"
    , items = List.map viewDep deps
    , footer =
        Just
          ( "/_elm/dependencies"
          , "Add more dependencies?"
          )
    }


viewTestDeps : Config.Deps Version -> Html msg
viewTestDeps deps =
  Skeleton.box
    { title = "Test Dependencies"
    , items = List.map viewDep deps
    , footer =
        Just
          ( "/_elm/test-dependencies"
          , "Add more test dependencies?"
          )
    }


viewDep : (Repo, Version) -> List (Html msg)
viewDep (repo, version) =
  [ div
      [ style "float" "left" ]
      [ Icon.package
      , a [ href (toPackageUrl repo version) ] [ text (Repo.toString repo) ]
      ]
  , div
      [ style "float" "right" ]
      [ text (Version.toString version) ]
  ]


toPackageUrl : Repo -> Version -> String
toPackageUrl name version =
  "http://package.elm-lang.org/packages"
  </> Repo.toString name
  </> Version.toString version


-- TODO - make this actually work!
exactify : Config.Deps a -> Config.Deps Version
exactify deps =
  List.map (\(name, _) -> (name, Version 0 0 0)) deps
