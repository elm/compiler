{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit
  ( Init(..)
  , initToReport
  --
  , Diff(..)
  , diffToReport
  --
  , Bump(..)
  , bumpToReport
  , newPackageOverview
  --
  , Publish(..)
  , publishToReport
  --
  , Install(..)
  , installToReport
  --
  , Solver(..)
  --
  , Outline(..)
  , OutlineProblem(..)
  --
  , Details(..)
  , PackageProblem(..)
  --
  , RegistryProblem(..)
  --
  , Make(..)
  , BuildProblem(..)
  , BuildProjectProblem(..)
  , makeToReport
  --
  , DocsProblem(..)
  --
  , toString
  , toStderr
  , toJson
  )
  where


import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.List as List
import qualified Data.NonEmptyList as NE
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP

import qualified Elm.Constraint as C
import qualified Elm.Magnitude as M
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Http
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode
import qualified Json.String as Json
import Parse.Primitives (Row, Col)
import qualified Reporting.Annotation as A
import Reporting.Doc ((<>))
import qualified Reporting.Doc as D
import qualified Reporting.Error.Import as Import
import qualified Reporting.Error.Json as Json
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Error as Error
import qualified Reporting.Render.Code as Code



-- INIT


data Init
  = InitNoSolution [Pkg.Name]
  | InitNoOfflineSolution [Pkg.Name]
  | InitSolverProblem Solver
  | InitAlreadyExists
  | InitRegistryProblem RegistryProblem


initToReport :: Init -> Help.Report
initToReport exit =
  case exit of
    InitNoSolution pkgs ->
      Help.report "NO SOLUTION" Nothing
        "I tried to create an elm.json with the following direct dependencies:"
        [ D.indent 4 $ D.vcat $
            map (D.dullyellow . D.fromChars . Pkg.toChars) pkgs
        , D.reflow $
            "I could not find compatible versions though! This should not happen, so please\
            \ ask around one of the community forums at https://elm-lang.org/community to learn\
            \ what is going on!"
        ]

    InitNoOfflineSolution pkgs ->
      Help.report "NO OFFLINE SOLUTION" Nothing
        (error "TODO InitNoOfflineSolution" pkgs)
        (error "TODO InitNoOfflineSolution" pkgs)
        -- note about Russia IP addresses?
        -- ask if they are on a plane or train

    InitSolverProblem solver ->
      error "TODO InitSolverProblem" solver

    InitAlreadyExists ->
      Help.report "EXISTING PROJECT" Nothing
        "You already have an elm.json file, so there is nothing for me to initialize!"
        [ D.fillSep
            ["Maybe",D.green (D.fromChars (D.makeLink "init")),"can","help"
            ,"you","figure","out","what","to","do","next?"
            ]
        ]

    InitRegistryProblem problem ->
      toRegistryProblemReport "PROBLEM LOADING PACKAGE LIST" problem $
        "I need the list of published packages before I can start initializing projects"



-- DIFF


data Diff
  = DiffNoOutline
  | DiffBadOutline Outline
  | DiffApplication
  | DiffNoExposed Pkg.Name
  | DiffUnpublished
  | DiffUnknownPackage Pkg.Name [Pkg.Name]
  | DiffUnknownVersion Pkg.Name V.Version [V.Version]
  | DiffDocsProblem V.Version DocsProblem
  | DiffMustHaveLatestRegistry RegistryProblem
  | DiffBadDetails Details
  | DiffBadBuild BuildProblem


diffToReport :: Diff -> Help.Report
diffToReport diff =
  case diff of
    DiffNoOutline ->
      Help.report "DIFF WHAT?" Nothing
        "I cannot find an elm.json so I am not sure what you want me to diff.\
        \ Normally you run `elm diff` from within a project!"
        [ D.reflow $ "If you are just curious to see a diff, try running this command:"
        , D.indent 4 $ D.green $ "elm diff elm/http 1.0.0 2.0.0"
        ]

    DiffBadOutline outline ->
      toOutlineReport outline

    DiffApplication ->
      Help.report "CANNOT DIFF APPLICATIONS" (Just "elm.json")
        "Your elm.json says this project is an application, but `elm diff` only works\
        \ with packages. That way there are previously published versions of the API to\
        \ diff against!"
        [ D.reflow $ "If you are just curious to see a diff, try running this command:"
        , D.indent 4 $ D.dullyellow $ "elm diff elm/http 1.0.0 2.0.0"
        ]

    DiffNoExposed pkg ->
      Help.report "NO EXPOSED MODULES" (Just "elm.json")
        "Your elm.json has no \"exposed-modules\" which means there is no public API at\
        \ all right now! Try adding some modules back to this field."
        []

    DiffUnpublished ->
      Help.report "UNPUBLISHED" Nothing
        "This package is not published yet. There is nothing to diff against!"
        []

    DiffUnknownPackage pkg suggestions ->
      Help.report "UNKNOWN PACKAGE" Nothing
        ( "I cannot find a package called:"
        )
        [ D.indent 4 $ D.red $ D.fromChars $ Pkg.toChars pkg
        , "Maybe you want one of these instead?"
        , D.indent 4 $ D.dullyellow $ D.vcat $ map (D.fromChars . Pkg.toChars) suggestions
        , "But check <https://package.elm-lang.org> to see all possibilities!"
        ]

    DiffUnknownVersion _pkg vsn realVersions ->
      Help.docReport "UNKNOWN VERSION" Nothing
        ( D.fillSep $
            [ "Version", D.red (D.fromVersion vsn)
            , "has", "never", "been", "published,", "so", "I"
            , "cannot", "diff", "against", "it."
            ]
        )
        [ "Here are all the versions that HAVE been published:"
        , D.indent 4 $ D.dullyellow $ D.vcat $
            let
              sameMajor v1 v2 = V._major v1 == V._major v2
              mkRow vsns = D.hsep $ map D.fromVersion vsns
            in
              map mkRow $ List.groupBy sameMajor (List.sort realVersions)
        , "Want one of those instead?"
        ]

    DiffDocsProblem version problem ->
      toDocsProblemReport problem $
        "I need the docs for " ++ V.toChars version ++ " to compute this diff"

    DiffMustHaveLatestRegistry problem ->
      toRegistryProblemReport "PROBLEM UPDATING PACKAGE LIST" problem $
        "I need the latest list of published packages before I do this diff"

    DiffBadDetails details ->
      error "TODO DiffBadDetails" details

    DiffBadBuild buildProblem ->
      error "TODO DiffBadBuild" buildProblem



-- BUMP


data Bump
  = BumpNoOutline
  | BumpBadOutline Outline
  | BumpApplication
  | BumpUnexpectedVersion V.Version [V.Version]
  | BumpMustHaveLatestRegistry RegistryProblem
  | BumpCannotFindDocs Pkg.Name V.Version DocsProblem


bumpToReport :: Bump -> Help.Report
bumpToReport bump =
  case bump of
    BumpNoOutline ->
      Help.report "BUMP WHAT?" Nothing
        "I cannot find an elm.json so I am not sure what you want me to bump."
        [ D.reflow $
            "Elm packages always have an elm.json that says current the version number. If\
            \ you run this command from a directory with an elm.json file, I will try to bump\
            \ the version in there based on the API changes."
        ]

    BumpBadOutline outline ->
      toOutlineReport outline

    BumpApplication ->
      Help.report "CANNOT BUMP APPLICATIONS" (Just "elm.json")
        "Your elm.json says this is an application. That means it cannot be published\
        \ on <https://package.elm-lang.org> and therefore has no version to bump!"
        []

    BumpUnexpectedVersion vsn versions ->
      Help.docReport "CANNOT BUMP" (Just "elm.json")
        ( D.fillSep
            ["Your","elm.json","says","I","should","bump","relative","to","version"
            ,D.red (D.fromVersion vsn) <> ","
            ,"but","I","cannot","find","that","version","on","<https://package.elm-lang.org>."
            ,"That","means","there","is","no","API","for","me","to","diff","against","and"
            ,"figure","out","if","these","are","MAJOR,","MINOR,","or","PATCH","changes."
            ]
        )
        [ D.fillSep $
            ["Try","bumping","again","after","changing","the",D.dullyellow "\"version\"","in","elm.json"]
            ++ if length versions == 1 then ["to:"] else ["to","one","of","these:"]
        , D.vcat $ map (D.green . D.fromVersion) versions
        ]

    BumpMustHaveLatestRegistry problem ->
      toRegistryProblemReport "PROBLEM UPDATING PACKAGE LIST" problem $
        "I need the latest list of published packages before I can bump any versions"

    BumpCannotFindDocs _ version problem ->
      toDocsProblemReport problem $
        "I need the docs for " ++ V.toChars version ++ " to compute the next version number"



-- OVERVIEW OF VERSIONING


newPackageOverview :: String
newPackageOverview =
  unlines
    [ "This package has never been published before. Here's how things work:"
    , ""
    , "  - Versions all have exactly three parts: MAJOR.MINOR.PATCH"
    , ""
    , "  - All packages start with initial version " ++ V.toChars V.one
    , ""
    , "  - Versions are incremented based on how the API changes:"
    , ""
    , "        PATCH = the API is the same, no risk of breaking code"
    , "        MINOR = values have been added, existing values are unchanged"
    , "        MAJOR = existing values have been changed or removed"
    , ""
    , "  - I will bump versions for you, automatically enforcing these rules"
    , ""
    ]



-- PUBLISH


data Publish
  = PublishNoOutline
  | PublishBadOutline Outline
  | PublishBadDetails Details
  | PublishMustHaveLatestRegistry RegistryProblem
  | PublishApplication
  | PublishNotInitialVersion V.Version
  | PublishAlreadyPublished V.Version
  | PublishInvalidBump V.Version V.Version
  | PublishBadBump V.Version V.Version M.Magnitude V.Version M.Magnitude
  | PublishNoSummary
  | PublishNoExposed
  | PublishNoReadme
  | PublishShortReadme
  | PublishNoLicense
  | PublishBuildProblem BuildProblem
  | PublishMissingTag V.Version
  | PublishCannotGetTag V.Version Http.Error
  | PublishCannotGetTagData V.Version String BS.ByteString
  | PublishCannotGetZip Http.Error
  | PublishCannotDecodeZip
  | PublishCannotGetDocs V.Version V.Version DocsProblem
  | PublishCannotRegister Http.Error
  | PublishNoGit
  | PublishLocalChanges V.Version
  --
  | PublishZipBadDetails Details
  | PublishZipApplication
  | PublishZipNoExposed
  | PublishZipBuildProblem BuildProblem


publishToReport :: Publish -> Help.Report
publishToReport publish =
  case publish of
    PublishNoOutline ->
      Help.report "PUBLISH WHAT?" Nothing
        "I cannot find an elm.json so I am not sure what you want me to publish."
        [ D.reflow $
            "Elm packages always have an elm.json that states the version number,\
            \ dependencies, exposed modules, etc."
        ]

    PublishBadOutline outline ->
      toOutlineReport outline

    PublishBadDetails problem ->
      error "TODO PublishBadDetails" problem

    PublishMustHaveLatestRegistry problem ->
      toRegistryProblemReport "PROBLEM UPDATING PACKAGE LIST" problem $
        "I need the latest list of published packages to make sure this is safe to publish"

    PublishApplication ->
      Help.report "UNPUBLISHABLE" Nothing "I cannot publish applications, only packages!" []

    PublishNotInitialVersion vsn ->
      Help.docReport "INVALID VERSION" Nothing
        ( D.fillSep
            ["I","cannot","publish"
            ,D.red (D.fromVersion vsn)
            ,"as","the","initial","version."
            ]
        )
        [ D.fillSep
            ["Change","it","to",D.green "1.0.0","which","is"
            ,"the","initial","version","for","all","Elm","packages."
            ]
        ]

    PublishAlreadyPublished vsn ->
      Help.docReport "ALREADY PUBLISHED" Nothing
        ( D.vcat
            [ D.fillSep
                [ "Version", D.green (D.fromVersion vsn)
                , "has", "already", "been", "published.", "You", "cannot"
                , "publish", "it", "again!"
                ]
            , "Try using the `bump` command:"
            ]
        )
        [ D.dullyellow $ D.indent 4 "elm bump"
        , D.reflow $
            "It computes the version number based on API changes, ensuring\
            \ that no breaking changes end up in PATCH releases!"
        ]

    PublishInvalidBump statedVersion latestVersion ->
      Help.docReport "INVALID VERSION" (Just "elm.json")
        ( D.fillSep $
            ["Your","elm.json","says","the","next","version","should","be"
            ,D.red (D.fromVersion statedVersion) <> ","
            ,"but","that","is","not","valid","based","on","the","previously"
            ,"published","versions."
            ]
        )
        [ D.fillSep $
            ["Change","the","version","back","to"
            ,D.green (D.fromVersion latestVersion)
            ,"which","is","the","most","recently","published","version."
            ,"From","there,","have","Elm","bump","the","version","by","running:"
            ]
        , D.indent 4 $ D.green "elm bump"
        , D.reflow $
            "If you want more insight on the API changes Elm detects, you\
            \ can run `elm diff` at this point as well."
        ]

    PublishBadBump old new magnitude realNew realMagnitude ->
      Help.docReport "INVALID VERSION" (Just "elm.json")
        (
          D.fillSep $
            ["Your","elm.json","says","the","next","version","should","be"
            ,D.red (D.fromVersion new) <> ","
            ,"indicating","a",D.fromChars (M.toChars magnitude)
            ,"change","to","the","public","API."
            ,"This","does","not","match","the","API","diff","given","by:"
            ]
        )
        [ D.indent 4 $ D.fromChars $
            "elm diff " ++ V.toChars old

        , D.fillSep $
            ["This","command","says","this","is","a"
            ,D.fromChars (M.toChars realMagnitude)
            ,"change,","so","the","next","version","should","be"
            ,D.green (D.fromVersion realNew) <> "."
            ,"Double","check","everything","to","make","sure","you"
            ,"are","publishing","what","you","want!"
            ]
        , D.reflow $
            "Also, next time use `elm bump` and I'll figure all this out for you!"
        ]

    PublishNoSummary ->
      Help.docReport "NO SUMMARY" (Just "elm.json")
        ( D.fillSep $
            [ "To", "publish", "a", "package,", "your", "elm.json", "must"
            , "have", "a", D.dullyellow "\"summary\"", "field", "that", "gives"
            , "a", "consice", "overview", "of", "your", "project."
            ]
        )
        [ D.reflow $
            "The summary must be less than 80 characters. It should describe\
            \ the concrete use of your package as clearly and as plainly as possible."
        ]

    PublishNoExposed ->
      Help.docReport "NO EXPOSED MODULES" (Just "elm.json")
        ( D.fillSep $
            [ "To", "publish", "a", "package,", "the"
            , D.dullyellow "\"exposed-modules\"", "field", "of", "your"
            , "elm.json", "must", "list", "at", "least", "one", "module."
            ]
        )
        [ D.reflow $
            "What is the point of a package that has no modules?!"
        ]

    PublishNoReadme ->
      toBadReadmeReport "NO README" $
        "Every published package must have a helpful README.md\
        \ file, but I do not see one in your project."

    PublishShortReadme ->
      toBadReadmeReport "SHORT README" $
        "This README.md is too short. Having more details will help\
        \ people assess your package quickly and fairly."

    PublishNoLicense ->
      Help.report "NO LICENSE FILE" (Just "LICENSE")
        "By publishing a package you are inviting the Elm community to build\
        \ upon your work. But without knowing your license, we have no idea if\
        \ that is legal!"
        [ D.reflow $
            "Once you pick an OSI approved license from <https://spdx.org/licenses/>,\
            \ you must share that choice in two places. First, the license\
            \ identifier must appear in your elm.json file. Second, the full\
            \ license text must appear in the root of your project in a file\
            \ named LICENSE. Add that file and you will be all set!"
        ]

    PublishBuildProblem buildProblem ->
      error "TODO PublishBuildProblem" buildProblem

    PublishMissingTag version ->
      let vsn = V.toChars version in
      Help.docReport "NO TAG" Nothing
        ( D.fillSep $
            [ "Packages", "must", "be", "tagged", "in", "git,", "but", "I"
            , "cannot", "find", "a", D.green (D.fromChars vsn), "tag."
            ]
        )
        [ D.vcat
            [ "These tags make it possible to find this specific version on GitHub."
            , "To tag the most recent commit and push it to GitHub, run this:"
            ]
        , D.indent 4 $ D.dullyellow $ D.vcat $ map D.fromChars $
            [ "git tag -a " ++ vsn ++ " -m \"new release\""
            , "git push origin " ++ vsn
            ]
        , "The -m flag is for a helpful message. Try to make it more informative!"
        ]

    PublishCannotGetTag version httpError ->
      case httpError of
        Http.BadHttp _ (HTTP.StatusCodeException response _)
          | HTTP.statusCode (HTTP.responseStatus response) == 404 ->
              let vsn = V.toChars version in
              Help.report "NO TAG ON GITHUB" Nothing
                ("You have version " ++ vsn ++ " tagged locally, but not on GitHub.")
                [ D.reflow
                    "Run the following command to make this tag available on GitHub:"
                , D.indent 4 $ D.dullyellow $ D.fromChars $
                    "git push origin " ++ vsn
                , D.reflow
                    "This will make it possible to find your code online based on the version number."
                ]

        _ ->
          toHttpErrorReport "PROBLEM VERIFYING TAG" httpError
            "I need to check that the version tag is registered on GitHub"

    PublishCannotGetTagData version url body ->
      Help.report "PROBLEM VERIFYING TAG" Nothing
        ("I need to check that version " ++ V.toChars version ++ " is tagged on GitHub, so I fetched:")
        [ D.indent 4 $ D.dullyellow $ D.fromChars url
        , D.reflow $
            "I got the data back, but it was not what I was expecting. The response\
            \ body contains " ++ show (BS.length body) ++ " bytes. Here is the "
            ++ if BS.length body <= 76 then "whole thing:" else "beginning:"
        , D.indent 4 $ D.dullyellow $ D.fromChars $
            if BS.length body <= 76
            then BS_UTF8.toString body
            else take 73 (BS_UTF8.toString body) ++ "..."
        , D.reflow $
            "Does this error keep showing up? Maybe there is something weird with your\
            \ internet connection. We have gotten reports that schools, businesses,\
            \ airports, etc. sometimes intercept requests and add things to the body\
            \ or change its contents entirely. Could that be the problem?"
        ]

    PublishCannotGetZip httpError ->
      toHttpErrorReport "PROBLEM DOWNLOADING CODE" httpError $
        "I need to check that folks can download and build the source code when they install this package"

    PublishCannotDecodeZip ->
      error "TODO PublishCannotDecodeZip"

    PublishCannotGetDocs old new docsProblem ->
      toDocsProblemReport docsProblem $
        "I need the docs for " ++ V.toChars old ++ " to verify that "
        ++ V.toChars new ++ " really does come next"

    PublishCannotRegister httpError ->
      toHttpErrorReport "PROBLEM PUBLISHING PACKAGE" httpError $
        "I need to send information about your package to the package website"

    PublishNoGit ->
      Help.report "NO GIT" Nothing
        "I searched your PATH environment variable for `git` and could not\
        \ find it. Is it available through your PATH?"
        [ D.reflow $
            "Who cares about this? Well, I currently use `git` to check if there\
            \ are any local changes in your code. Local changes are a good sign\
            \ that some important improvements have gotten mistagged, so this\
            \ check can be extremely helpful for package authors!"
        , D.toSimpleNote $
            "We plan to do this without the `git` binary in a future release."
        ]

    PublishLocalChanges version ->
      let vsn = V.toChars version in
      Help.docReport "LOCAL CHANGES" Nothing
        ( D.fillSep $
            [ "The", "code", "tagged", "as", D.green (D.fromChars vsn), "in"
            , "git", "does", "not", "match", "the", "code", "in", "your"
            , "working", "directory.", "This", "means", "you", "have"
            , "commits", "or", "local", "changes", "that", "are", "not"
            , "going", "to", "be", "published!"
            ]
        )
        [ D.toSimpleNote $
            "If you are sure everything is in order, you can run `git checkout "
            ++ vsn ++ "` and publish your code from there."
        ]

    PublishZipBadDetails details ->
      error "TODO PublishZipBadDetails" details

    PublishZipApplication ->
      error "TODO PublishZipApplication"

    PublishZipNoExposed ->
      error "TODO PublishZipNoExposed"

    PublishZipBuildProblem buildProblem ->
      error "TODO PublishZipBuildProblem" buildProblem


toBadReadmeReport :: String -> String -> Help.Report
toBadReadmeReport title summary =
  Help.report title (Just "README.md") summary
    [ D.reflow $
        "When people look at your README, they are wondering:"
    , D.vcat
        [ "  - What does this package even do?"
        , "  - Will it help me solve MY problems?"
        ]
    , D.reflow $
        "So I recommend starting your README with a small example of the\
        \ most common usage scenario. Show people what they can expect if\
        \ they learn more!"
    , D.toSimpleNote $
        "By publishing your package, you are inviting people to invest time\
        \ in understanding your work. Spending an hour to communicate your\
        \ knowledge more clearly can save the community days or weeks of time\
        \ in aggregate, and saving time in aggregate is the whole point of\
        \ publishing packages! People really appreciate it, and it makes the\
        \ whole ecosystem feel nicer!"
    ]



-- DOCS


data DocsProblem
  = DP_Http Http.Error
  | DP_Data String BS.ByteString
  | DP_Cache


toDocsProblemReport :: DocsProblem -> String -> Help.Report
toDocsProblemReport problem context =
  case problem of
    DP_Http httpError ->
      toHttpErrorReport "PROBLEM LOADING DOCS" httpError context

    DP_Data url body ->
      Help.report "PROBLEM LOADING DOCS" Nothing (context ++ ", so I fetched:")
        [ D.indent 4 $ D.dullyellow $ D.fromChars url
        , D.reflow $
            "I got the data back, but it was not what I was expecting. The response\
            \ body contains " ++ show (BS.length body) ++ " bytes. Here is the "
            ++ if BS.length body <= 76 then "whole thing:" else "beginning:"
        , D.indent 4 $ D.dullyellow $ D.fromChars $
            if BS.length body <= 76
            then BS_UTF8.toString body
            else take 73 (BS_UTF8.toString body) ++ "..."
        , D.reflow $
            "Does this error keep showing up? Maybe there is something weird with your\
            \ internet connection. We have gotten reports that schools, businesses,\
            \ airports, etc. sometimes intercept requests and add things to the body\
            \ or change its contents entirely. Could that be the problem?"
        ]

    DP_Cache ->
      Help.report "PROBLEM LOADING DOCS" Nothing (context ++ ", but the local copy seems to be corrupted.")
        [ D.reflow $
            "I deleted the cached version, so the next run should download a fresh copy of\
            \ the docs. Hopefully that will get you unstuck, but it will not resolve the root\
            \ problem if, for example, a 3rd party editor plugin is modifing cached files\
            \ for some reason."
        ]



-- INSTALL


data Install
  = InstallNoProject
  | InstallNoArgs FilePath
  | InstallNoOnlineSolution
  | InstallNoOfflineSolution
  | InstallHadSolverTrouble Solver
  | InstallUnknownPackageOnline Pkg.Name [Pkg.Name]
  | InstallUnknownPackageOffline Pkg.Name [Pkg.Name]
  | InstallHasBadDetails Details


installToReport :: Install -> Help.Report
installToReport exit =
  case exit of
    InstallNoProject ->
      Help.report "NEW PROJECT?" Nothing
        "Are you trying to start a new project? Try this command instead:"
        [ D.indent 4 $ D.green "elm init"
        , D.reflow "It will help you get started!"
        ]

    InstallNoArgs elmHome ->
      Help.report "INSTALL WHAT?" Nothing
        "I am expecting commands like:"
        [ D.green $ D.indent 4 $ D.vcat $
            [ "elm install elm/http"
            , "elm install elm/json"
            , "elm install elm/random"
            ]
        , D.toFancyHint
            ["In","JavaScript","folks","run","`npm install`","to","start","projects."
            ,"\"Gotta","download","everything!\"","But","why","download","packages"
            ,"again","and","again?","Instead,","Elm","caches","packages","in"
            ,D.dullyellow (D.fromChars elmHome)
            ,"so","each","one","is","downloaded","and","built","ONCE","on","your","machine."
            ,"Elm","projects","check","that","cache","before","trying","the","internet."
            ,"This","reduces","build","times,","reduces","server","costs,","and","makes","it"
            ,"easier","to","work","offline.","As","a","result"
            ,D.dullcyan "elm install","is","only","for","adding","dependencies","to","elm.json,"
            ,"whereas",D.dullcyan "elm make","is","in","charge","of","gathering","dependencies"
            ,"and","building","everything.","So","maybe","try",D.green "elm make","instead?"
            ]
        ]

    InstallNoOnlineSolution ->
      Help.report "UNSOLVABLE DEPENDENCIES" (Just "elm.json") -- TODO recommend using solver in `elm reactor`
        "This usually happens if you try to modify dependency constraints by\
        \ hand. I recommend deleting any dependency you added recently (or all\
        \ of them if things are bad) and then adding them again with:"
        [ D.indent 4 $ D.green "elm install"
        , D.reflow $
            "And do not be afaid to ask for help on Slack if you get stuck!"
        ]

    InstallNoOfflineSolution ->
      error "TODO InstallNoOfflineSolution"

    InstallHadSolverTrouble solver ->
      error "TODO InstallHadSolverTrouble" solver

    InstallUnknownPackageOnline pkg suggestions ->
      Help.docReport "UNKNOWN PACKAGE" Nothing
        (
          D.fillSep
            ["I","cannot","find","a","package","named",D.red (D.fromPackage pkg) <> "."]
        )
        [ D.reflow $
            "I looked through https://package.elm-lang.org for packages with similar names\
            \ and found these:"
        , D.indent 4 $ D.dullyellow $ D.vcat $ map D.fromPackage suggestions
        , D.reflow $ "Maybe you want one of these instead?"
        ]

    InstallUnknownPackageOffline pkg suggestions ->
      Help.docReport "UNKNOWN PACKAGE" Nothing
        (
          D.fillSep
            ["I","cannot","find","a","package","named",D.red (D.fromPackage pkg) <> "."]
        )
        [ D.reflow $
            "I could not connect to https://package.elm-lang.org though, so new packages may\
            \ have been published since I last updated my local cache of package names."
        , D.reflow $
            "Looking through the locally cached names, the closest ones are:"
        , D.indent 4 $ D.dullyellow $ D.vcat $ map D.fromPackage suggestions
        , D.reflow $ "Maybe you want one of these instead?"
        ]

    InstallHasBadDetails _ ->
      error "TODO InstallHasBadDetails"


{- TODO detect if library has no 0.19.0 version. Maybe do this on website though?
  Help.report "OLD DEPENDENCIES" (Just "elm.json")
    ( "The following packages do not work with Elm " ++ V.toChars V.compiler ++ " right now:"
    )
    [ D.indent 4 $ D.vcat $ map (D.red . D.fromPackage) badPackages
    , D.reflow $
        "This may be because it is not upgraded yet. It may be because a\
        \ better solution came along, so there was no need to upgrade it.\
        \ Etc. Try asking around on Slack to learn more about the topic."
    , D.toSimpleNote
        "Whatever the case, please be kind to the relevant package authors! Having\
        \ friendly interactions with users is great motivation, and conversely, getting\
        \ berated by strangers on the internet sucks your soul dry. Furthermore, package\
        \ authors are humans with families, friends, jobs, vacations, responsibilities,\
        \ goals, etc. They face obstacles outside of their technical work you will never\
        \ know about, so please assume the best and try to be patient and supportive!"
    ]
-}



-- SOLVER


data Solver
  = SolverBadCacheData Pkg.Name V.Version
  | SolverBadHttpData Pkg.Name V.Version
  | SolverBadHttp Http.Error



-- OUTLINE


data Outline
  = OutlineHasBadStructure (Decode.Error OutlineProblem)
  | OutlineHasBadSrcDirs FilePath [FilePath]
  | OutlineNoPkgCore
  | OutlineNoAppCore
  | OutlineNoAppJson


data OutlineProblem
  = OP_BadType
  | OP_BadPkgName Row Col
  | OP_BadVersion Row Col
  | OP_BadConstraint C.Error
  | OP_BadModuleName Row Col
  | OP_BadModuleHeaderTooLong
  | OP_BadDependencyName Row Col
  | OP_BadLicense Json.String [Json.String]
  | OP_BadSummaryTooLong
  | OP_NoSrcDirs



-- DETAILS


data Details
  = DetailsCorrupt
  | DetailsNoSolution
  | DetailsNoOfflineSolution
  | DetailsSolverProblem Solver
  | DetailsBadElmInPkg C.Constraint
  | DetailsBadElmInAppOutline V.Version
  | DetailsHandEditedDependencies
  | DetailsBadOutline Outline
  | DetailsCannotGetRegistry RegistryProblem
  | DetailsCannotBuildPackage Pkg.Name V.Version PackageProblem


data PackageProblem
  = PP_BadEndpointRequest Http.Error
  | PP_BadEndpointContent
  | PP_BadArchiveRequest Http.Error
  | PP_BadArchiveContent
  | PP_BadArchiveHash String String



-- REGISTRY PROBLEM


data RegistryProblem
  = RP_Http Http.Error
  | RP_Data String BS.ByteString


toRegistryProblemReport :: String -> RegistryProblem -> String -> Help.Report
toRegistryProblemReport title problem context =
  case problem of
    RP_Http err ->
      toHttpErrorReport title err context

    RP_Data url body ->
      Help.report title Nothing (context ++ ", so I fetched:")
        [ D.indent 4 $ D.dullyellow $ D.fromChars url
        , D.reflow $
            "I got the data back, but it was not what I was expecting. The response\
            \ body contains " ++ show (BS.length body) ++ " bytes. Here is the "
            ++ if BS.length body <= 76 then "whole thing:" else "beginning:"
        , D.indent 4 $ D.dullyellow $ D.fromChars $
            if BS.length body <= 76
            then BS_UTF8.toString body
            else take 73 (BS_UTF8.toString body) ++ "..."
        , D.reflow $
            "Does this error keep showing up? Maybe there is something weird with your\
            \ internet connection. We have gotten reports that schools, businesses,\
            \ airports, etc. sometimes intercept requests and add things to the body\
            \ or change its contents entirely. Could that be the problem?"
        ]


toHttpErrorReport :: String -> Http.Error -> String -> Help.Report
toHttpErrorReport title err context =
  let
    toHttpReport intro url details =
      Help.report title Nothing intro $
        D.indent 4 (D.dullyellow (D.fromChars url)) : details
  in
  case err of
    Http.BadUrl url reason ->
      toHttpReport (context ++ ", so I wanted to fetch:") url
        [ D.reflow $ "But my HTTP library is saying this is not a valid URL. It is saying:"
        , D.indent 4 $ D.fromChars reason
        , D.reflow $
            "This may indicate that there is some problem in the compiler, so please open an\
            \ issue at https://github.com/elm/compiler/issues listing your operating system, Elm\
            \ version, the command you ran, the terminal output, and any additional information\
            \ that might help others reproduce the error."
        ]

    Http.BadHttp url httpExceptionContent ->
      case httpExceptionContent of
        HTTP.StatusCodeException response _ ->
          let
            (HTTP.Status code message) = HTTP.responseStatus response
          in
          toHttpReport (context ++ ", so I tried to fetch:") url
            [ D.fillSep $
                ["But","it","came","back","as",D.red (D.fromInt code)]
                ++ map D.fromChars (words (BS_UTF8.toString message))
            , D.reflow $
                "This may mean some online endpoint changed in an unexpected way, so if does not\
                \ seem like something on your side is causing this (e.g. firewall) please report\
                \ this to https://github.com/elm/compiler/issues with your operating system, Elm\
                \ version, the command you ran, the terminal output, and any additional information\
                \ that can help others reproduce the error!"
            ]

        HTTP.TooManyRedirects responses ->
          toHttpReport (context ++ ", so I tried to fetch:") url
            [ D.reflow $ "But I gave up after following these " ++ show (length responses) ++ " redirects:"
            , D.indent 4 $ D.vcat $ map toRedirectDoc responses
            , D.reflow $
                "Is it possible that your internet connection intercepts certain requests? That\
                \ sometimes causes problems for folks in schools, businesses, airports, hotels,\
                \ and certain countries. Try asking for help locally or in a community forum!"
            ]

        otherException ->
          toHttpReport (context ++ ", so I tried to fetch:") url
            [ D.reflow $ "But my HTTP library is giving me the following error message:"
            , D.indent 4 $ D.fromChars (show otherException)
            , D.reflow $
                "Are you somewhere with a slow internet connection? Or no internet?\
                \ Does the link I am trying to fetch work in your browser? Maybe the\
                \ site is down? Does your internet connection have a firewall that\
                \ blocks certain domains? It is usually something like that!"
            ]

    Http.BadMystery url someException ->
      toHttpReport (context ++ ", so I tried to fetch:") url
        [ D.reflow $ "But I ran into something weird! I was able to extract this error message:"
        , D.indent 4 $ D.fromChars (show someException)
        , D.reflow $
            "Is it possible that your internet connection intercepts certain requests? That\
            \ sometimes causes problems for folks in schools, businesses, airports, hotels,\
            \ and certain countries. Try asking for help locally or in a community forum!"
        ]


toRedirectDoc :: HTTP.Response body -> D.Doc
toRedirectDoc response =
  let
    (HTTP.Status code message) = HTTP.responseStatus response
  in
  case List.lookup HTTP.hLocation (HTTP.responseHeaders response) of
    Just loc -> D.red (D.fromInt code) <> " - " <> D.fromChars (BS_UTF8.toString loc)
    Nothing  -> D.red (D.fromInt code) <> " - " <> D.fromChars (BS_UTF8.toString message)



-- MAKE


data Make
  = MakeNeedsOutline
  | MakeCannotOptimizeAndDebug
  | MakeBlockedByDetailsProblem Details
  | MakeAppNeedsFileNames
  | MakePkgNeedsExposing
  | MakeMultipleFilesIntoHtml
  | MakeNonMainFilesIntoJavaScript ModuleName.Raw [ModuleName.Raw]
  | MakeCannotOptimizeDebugValues ModuleName.Raw [ModuleName.Raw]
  | MakeCannotBuild BuildProblem
  | MakeCannotLoadArtifacts


data BuildProblem
  = BuildModuleProblems Error.Module [Error.Module]
  | BuildProjectProblem BuildProjectProblem


data BuildProjectProblem
  = BP_PathUnknown FilePath
  | BP_WithBadExtension FilePath
  | BP_WithAmbiguousSrcDir FilePath FilePath
  | BP_MainPathDuplicate FilePath FilePath
  | BP_MainNameDuplicate ModuleName.Raw FilePath FilePath
  | BP_CannotLoadDependencies
  | BP_Cycle ModuleName.Raw [ModuleName.Raw]
  | BP_MissingExposed (NE.List (ModuleName.Raw, Import.Problem))



-- RENDERERS


toString :: Help.Report -> String
toString report =
  Help.toString (Help.reportToDoc report)


toStderr :: Help.Report -> IO ()
toStderr report =
  Help.toStderr (Help.reportToDoc report)


toJson :: Help.Report -> Encode.Value
toJson report =
  Help.reportToJson report



-- DETAILS TO REPORT


detailsToReport :: Details -> Help.Report
detailsToReport details =
  case details of
    DetailsCorrupt ->
      error "TODO DetailsCorrupt"

    DetailsBadElmInPkg constraint ->
      Help.report "ELM VERSION MISMATCH" (Just "elm.json")
        "Your elm.json says this package needs a version of Elm in this range:"
        [ D.indent 4 $ D.dullyellow $ D.fromChars $ C.toChars constraint
        , D.fillSep
            [ "But", "you", "are", "using", "Elm"
            , D.red (D.fromVersion V.compiler)
            , "right", "now."
            ]
        ]

    DetailsBadElmInAppOutline version ->
      Help.report "ELM VERSION MISMATCH" (Just "elm.json")
        "Your elm.json says this application needs a different version of Elm."
        [ D.fillSep
            [ "It", "requires"
            , D.green (D.fromVersion version) <> ","
            , "but", "you", "are", "using"
            , D.red (D.fromVersion V.compiler)
            , "right", "now."
            ]
        ]

    DetailsHandEditedDependencies ->
      error "TODO DetailsHandEditedDependencies"

    DetailsBadOutline outline ->
      toOutlineReport outline

    DetailsCannotGetRegistry problem ->
      toRegistryProblemReport "PROBLEM LOADING PACKAGE LIST" problem $
        "I need the list of published packages to figure out if your project has compatible dependencies"

    DetailsCannotBuildPackage name version problem ->
      error "TODO DetailsCannotBuildPackage" name version problem

    DetailsNoSolution ->
      error "TODO DetailsNoSolution"

    DetailsNoOfflineSolution ->
      error "TODO DetailsNoOfflineSolution"

    DetailsSolverProblem _ ->
      error "TODO DetailsSolverProblem"



makeToReport :: Make -> Help.Report
makeToReport make =
  case make of
    MakeNeedsOutline ->
      Help.report "NO elm.json FILE" Nothing
        "It looks like you are starting a new Elm project. Very exciting! Try running:"
        [ D.indent 4 $ D.green $ "elm init"
        , D.reflow $
            "It will help you get set up. It is really simple!"
        ]

    MakeCannotOptimizeAndDebug ->
      Help.docReport "CLASHING FLAGS" Nothing
        ( D.fillSep
            ["I","cannot","compile","with",D.red "--optimize","and"
            ,D.red "--debug","at","the","same","time."
            ]
        )
        [ D.reflow
            "I need to take away information to optimize things, and I need to\
            \ add information to add the debugger. It is impossible to do both\
            \ at once though! Pick just one of those flags and it should work!"
        ]

    MakeBlockedByDetailsProblem detailsProblem ->
      detailsToReport detailsProblem

    MakeAppNeedsFileNames ->
      Help.report "NO INPUT" Nothing
        "What should I make though? I need a specific file like this:"
        [ D.indent 4 $ D.green "elm make src/Main.elm"
        , D.reflow
            "However many files you give, I will create one JS file out of them."
        ]

    MakePkgNeedsExposing ->
      error "TODO MakePkgNeedsExposing"

    MakeMultipleFilesIntoHtml ->
      error "TODO MakeMultipleFilesIntoHtml"

    MakeNonMainFilesIntoJavaScript _ _ ->
      error "TODO MakeNonMainFilesIntoJavaScript"

    MakeCannotOptimizeDebugValues m ms ->
      Help.report "DEBUG REMNANTS" Nothing
        "There are uses of the `Debug` module in the following modules:"
        [ D.indent 4 $ D.red $ D.vcat $ map (D.fromChars . ModuleName.toChars) (m:ms)
        , D.reflow "But the --optimize flag only works if all `Debug` functions are removed!"
        , D.toSimpleNote $
            "The issue is that --optimize strips out info needed by `Debug` functions.\
            \ Here are two examples:"
        , D.indent 4 $ D.reflow $
            "(1) It shortens record field names. This makes the generated JavaScript is\
            \ smaller, but `Debug.toString` cannot know the real field names anymore."
        , D.indent 4 $ D.reflow $
            "(2) Values like `type Height = Height Float` are unboxed. This reduces\
            \ allocation, but it also means that `Debug.toString` cannot tell if it is\
            \ looking at a `Height` or `Float` value."
        , D.reflow $
            "There are a few other cases like that, and it will be much worse once we start\
            \ inlining code. That optimization could move `Debug.log` and `Debug.todo` calls,\
            \ resulting in unpredictable behavior. I hope that clarifies why this restriction\
            \ exists!"
        ]

    MakeCannotBuild buildProblem ->
      case buildProblem of
        BuildProjectProblem bp ->
          case bp of
            BP_PathUnknown _ ->
              error "TODO BP_PathUnknown"

            BP_WithBadExtension _ ->
              error "TODO BP_WithBadExtension"

            BP_WithAmbiguousSrcDir _ _ ->
              error "TODO BP_WithAmbiguousSrcDir"

            BP_MainPathDuplicate _ _ ->
              error "TODO BP_MainPathDuplicate"

            BP_MainNameDuplicate _ _ _ ->
              error "TODO BP_MainNameDuplicate"

            BP_CannotLoadDependencies ->
              error "TODO BP_CannotLoadDependencies"

            BP_Cycle name names ->
              Help.report "IMPORT CYCLE" Nothing
                "Your module imports form a cycle:"
                [ D.cycle 4 name names
                , D.reflow $
                    "Learn more about why this is disallowed and how to break cycles here:"
                    ++ D.makeLink "import-cycles"
                ]

            BP_MissingExposed _ ->
              error "TODO BP_MissingExposed"

        BuildModuleProblems p ps ->
          Help.compilerReport p ps

    MakeCannotLoadArtifacts ->
      error "TODO MakeCannotLoadArtifacts"



-- OUTLINE


toOutlineReport :: Outline -> Help.Report
toOutlineReport problem =
  case problem of
    OutlineHasBadStructure decodeError ->
      Json.toReport "elm.json" (Json.FailureToReport toOutlineProblemReport) decodeError $
        Json.ExplicitReason "I ran into a problem with your elm.json file."

    OutlineHasBadSrcDirs dir dirs ->
      case dirs of
        [] ->
          Help.report "MISSING SOURCE DIRECTORY" (Just "elm.json")
            "I need a valid elm.json file, but the \"source-directories\" field lists the following directory:"
            [ D.indent 4 $ D.dullyellow $ D.fromChars dir
            , D.reflow $
                "I cannot find it though. Is it missing? Is there a typo?"
            ]

        _:_ ->
          Help.report "MISSING SOURCE DIRECTORIES" (Just "elm.json")
            "I need a valid elm.json file, but the \"source-directories\" field lists the following directories:"
            [ D.indent 4 $ D.dullyellow $ D.fromChars dir
            , D.reflow $
                "I cannot find them though. Are they missing? Are there typos?"
            ]

    OutlineNoPkgCore ->
      Help.report "MISSING DEPENDENCY" (Just "elm.json")
        "I need a valid elm.json file, so I need to see an \"elm/core\" dependency in there. Try running:"
        [ D.indent 4 $ D.green $ "elm install elm/core"
        , D.reflow "I need it for the default imports that make `List` and `Maybe` available."
        ]

    OutlineNoAppCore ->
      Help.report "MISSING DEPENDENCY" (Just "elm.json")
        "I need a valid elm.json file, so I need to see an \"elm/core\" dependency in there. Try running:"
        [ D.indent 4 $ D.green $ "elm install elm/core"
        , D.reflow "It has some supporting code that is needed by every Elm application!"
        ]

    OutlineNoAppJson ->
      Help.report "MISSING DEPENDENCY" (Just "elm.json")
        "I need a valid elm.json file, so I need to see an \"elm/json\" dependency in there. Try running:"
        [ D.indent 4 $ D.green $ "elm install elm/json"
        , D.reflow "It helps me handle flags and ports."
        ]


toOutlineProblemReport :: FilePath -> Code.Source -> Json.Context -> A.Region -> OutlineProblem -> Help.Report
toOutlineProblemReport path source _ region problem =
  let
    toHighlight row col =
      Just $ A.Region (A.Position row col) (A.Position row col)

    toSnippet title highlight pair =
      Help.jsonReport title (Just path) $
        Code.toSnippet source region highlight pair
  in
  case problem of
    OP_BadType ->
      toSnippet "UNEXPECTED TYPE" Nothing
        ( D.reflow $
            "I got stuck while reading your elm.json file. I cannot handle a \"type\" like this:"
        , D.fillSep
            ["Try","changing","the","\"type\"","to"
            ,D.green "\"application\"","or",D.green "\"package\"","instead."
            ]
        )

    OP_BadPkgName row col ->
      toSnippet "INVALID PACKAGE NAME" (toHighlight row col)
        ( D.reflow $
            "I got stuck while reading your elm.json file. I ran into trouble with the package name:"
        , D.stack
            [ D.fillSep
                ["Package","names","are","always","written","as"
                ,D.green "\"author/project\""
                ,"so","I","am","expecting","to","see","something","like:"
                ]
            , D.dullyellow $ D.indent 4 $ D.vcat $
                [ "\"mdgriffith/elm-ui\""
                , "\"w0rm/elm-physics\""
                , "\"Microsoft/elm-json-tree-view\""
                , "\"FordLabs/elm-star-rating\""
                , "\"1602/json-schema\""
                ]
            , D.reflow
                "The author name should match your GitHub name exactly, and the project name\
                \ needs to follow these rules:"
            , D.indent 4 $ D.vcat $
                [ "+--------------------------------------+-----------+-----------+"
                , "| RULE                                 | BAD       | GOOD      |"
                , "+--------------------------------------+-----------+-----------+"
                , "| only lower case, digits, and hyphens | elm-HTTP  | elm-http  |"
                , "| no leading digits                    | 3D        | elm-3d    |"
                , "| no non-ASCII characters              | elm-bjørn | elm-bear  |"
                , "| no underscores                       | elm_ui    | elm-ui    |"
                , "| no double hyphens                    | elm--hash | elm-hash  |"
                , "| no starting or ending hyphen         | -elm-tar- | elm-tar   |"
                , "+--------------------------------------+-----------+-----------+"
                ]
            , D.toSimpleNote $
                "These rules only apply to the project name, so you should never need\
                \ to change your GitHub name!"
            ]
        )

    OP_BadVersion row col ->
      toSnippet "PROBLEM WITH VERSION" (toHighlight row col)
        ( D.reflow $
            "I got stuck while reading your elm.json file. I was expecting a version number here:"
        , D.fillSep
            ["I","need","something","like",D.green "\"1.0.0\"","or",D.green "\"2.0.4\""
            ,"that","explicitly","states","all","three","numbers!"
            ]
        )

    OP_BadConstraint constraintError ->
      case constraintError of
        C.BadFormat row col ->
          toSnippet "PROBLEM WITH CONSTRAINT" (toHighlight row col)
            ( D.reflow $
                "I got stuck while reading your elm.json file. I do not understand this version constraint:"
            , D.stack
                [ D.fillSep
                    ["I","need","something","like",D.green "\"1.0.0 <= v < 2.0.0\""
                    ,"that","explicitly","lists","the","lower","and","upper","bounds."
                    ]
                , D.toSimpleNote $
                    "The spaces in there are required! Taking them out will confuse me. Adding\
                    \ extra spaces confuses me too. I recommend starting with a valid example\
                    \ and just changing the version numbers."
                ]
            )

        C.InvalidRange before after ->
          if before == after then
            toSnippet "PROBLEM WITH CONSTRAINT" Nothing
              ( D.reflow $
                  "I got stuck while reading your elm.json file. I ran into an invalid version constraint:"
              , D.fillSep
                  ["Elm","checks","that","all","package","APIs","follow","semantic","versioning,"
                  ,"so","it","is","best","to","use","wide","constraints.","I","recommend"
                  ,D.green $ "\"" <> D.fromVersion before <> " <= v < " <> D.fromVersion (V.bumpMajor after) <> "\""
                  ,"since","it","is","guaranteed","that","breaking","API","changes","cannot"
                  ,"happen","in","any","of","the","versions","in","that","range."
                  ]
              )

          else
            toSnippet "PROBLEM WITH CONSTRAINT" Nothing
              ( D.reflow $
                  "I got stuck while reading your elm.json file. I ran into an invalid version constraint:"
              , D.fillSep
                  ["Maybe","you","want","something","like"
                  ,D.green $ "\"" <> D.fromVersion before <> " <= v < " <> D.fromVersion (V.bumpMajor before) <> "\""
                  ,"instead?","Elm","checks","that","all","package","APIs","follow","semantic"
                  ,"versioning,","so","it","is","guaranteed","that","breaking","API","changes"
                  ,"cannot","happen","in","any","of","the","versions","in","that","range."
                  ]
              )

    OP_BadModuleName row col ->
      toSnippet "PROBLEM WITH MODULE NAME" (toHighlight row col)
        ( D.reflow $
            "I got stuck while reading your elm.json file. I was expecting a module name here:"
        , D.fillSep
            ["I","need","something","like",D.green "\"Html.Events\""
            ,"or",D.green "\"Browser.Navigation\""
            ,"where","each","segment","starts","with","a","capital"
            ,"letter","and","the","segments","are","separated","by","dots."
            ]
        )

    OP_BadModuleHeaderTooLong ->
      toSnippet "HEADER TOO LONG" Nothing
        ( D.reflow $
            "I got stuck while reading your elm.json file. This section header is too long:"
        , D.fillSep
            ["I","need","it","to","be"
            ,D.green "under",D.green "20",D.green "characters"
            ,"so","it","renders","nicely","on","the","package","website!"
            ]
        )

    OP_BadDependencyName row col ->
      toSnippet "PROBLEM WITH DEPENDENCY NAME" (toHighlight row col)
        ( D.reflow $
            "I got stuck while reading your elm.json file. There is something wrong with this dependency name:"
        , D.stack
            [ D.fillSep
                ["Package","names","always","include","the","name","of","the","author,"
                ,"so","I","am","expecting","to","see","dependencies","like"
                ,D.dullyellow "\"mdgriffith/elm-ui\"","and"
                ,D.dullyellow "\"Microsoft/elm-json-tree-view\"" <> "."
                ]
            , D.fillSep $
                ["I","generally","recommend","finding","the","package","you","want","on"
                ,"the","package","website,","and","installing","it","with","the"
                ,D.green "elm install","command!"
                ]
            ]
        )

    OP_BadLicense _ suggestions ->
      toSnippet "UNKNOWN LICENSE" Nothing
        ( D.reflow $
            "I got stuck while reading your elm.json file. I do not know about this type of license:"
        ,
          D.stack
            [ D.fillSep
                ["Elm","packages","generally","use"
                ,D.green "\"BSD-3-Clause\"","or",D.green "\"MIT\"" <> ","
                ,"but","I","accept","any","OSI","approved","SPDX","license."
                ,"Here","some","that","seem","close","to","what","you","wrote:"
                ]
            , D.indent 4 $ D.dullyellow $ D.vcat $ map (D.fromChars . Json.toChars) suggestions
            , D.reflow $
                "Check out https://spdx.org/licenses/ for the full list of options."
            ]
        )

    OP_BadSummaryTooLong ->
      toSnippet "SUMMARY TOO LONG" Nothing
        ( D.reflow $
            "I got stuck while reading your elm.json file. Your \"summary\" is too long:"
        , D.fillSep
            ["I","need","it","to","be"
            ,D.green "under",D.green "80",D.green "characters"
            ,"so","it","renders","nicely","on","the","package","website!"
            ]
        )

    OP_NoSrcDirs ->
      toSnippet "NO SOURCE DIRECTORIES" Nothing
        ( D.reflow $
            "I got stuck while reading your elm.json file. You do not have any \"source-directories\" listed here:"
        , D.fillSep
            ["I","need","something","like",D.green "[\"src\"]"
            ,"so","I","know","where","to","look","for","your","modules!"
            ]
        )
