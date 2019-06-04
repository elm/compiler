{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit
  ( Exit(..)
  --
  , Init(..)
  , initToReport
  --
  , Diff(..)
  , diffToReport
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
  , RegistryProblem(..)
  , PackageProblem(..)
  --
  , Make(..)
  , BuildProblem(..)
  , BuildProjectProblem(..)
  , makeToReport
  --
  , DocsProblem(..)
  , toString
  , toStderr
  , toJson
  )
  where


import qualified Data.List as List
import qualified Data.NonEmptyList as NE
import qualified Data.Utf8 as Utf8

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
import Reporting.Doc ((<>))
import qualified Reporting.Doc as D
import qualified Reporting.Error.Import as Import
import qualified Reporting.Error.Json as Json
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Error as Error



-- ALL POSSIBLE ERRORS


data Exit
  -- bump
  = BumpApplication
  | BumpUnexpectedVersion V.Version [V.Version]
  | BumpMustHaveLatestRegistry RegistryProblem
  | BumpCannotFindDocs Pkg.Name V.Version DocsProblem
  -- publish
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
  | PublishCannotGetTag Http.Error
  | PublishCannotGetZip Http.Error
  | PublishCannotDecodeZip
  | PublishCannotGetDocs DocsProblem
  | PublishCannotRegister Http.Error
  | PublishMissingTag V.Version
  | PublishNoGit
  | PublishLocalChanges V.Version


data DocsProblem
  = DP_Http Http.Error
  | DP_Data
  | DP_Cache



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

    InitRegistryProblem _ ->
      error "TODO InitRegistryProblem"



-- DIFF


data Diff
  = DiffNoOutline
  | DiffApplication
  | DiffUnpublished
  | DiffUnknownPackage Pkg.Name [Pkg.Name]
  | DiffUnknownVersion Pkg.Name V.Version [V.Version]
  | DiffDocsProblem DocsProblem
  | DiffBadRegistry RegistryProblem
  | DiffBadDetails Details


diffToReport :: Diff -> Help.Report
diffToReport diff =
  case diff of
    DiffNoOutline ->
      Help.report "DIFF WHAT?" Nothing
        "I cannot find an elm.json so I am not sure what you want me to diff.\
        \ Normally you run `elm diff` from within a project!"
        [ D.reflow $ "If you are just curious to see a diff, try running this command:"
        , D.indent 4 $ D.green $ "elm diff elm/html 5.1.1 6.0.0"
        ]

    DiffApplication ->
      Help.report "CANNOT DIFF APPLICATIONS" Nothing
        "It looks like you are running `elm diff` on an application, but it\
        \ only makes sense for packages that have been published. That way\
        \ there is a fixed API to diff against."
        [ D.reflow $ "If you are just curious to see a diff, try running this command:"
        , D.indent 4 $ D.green $ "elm diff elm/html 5.1.1 6.0.0"
        ]

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

    DiffDocsProblem problem ->
      error "TODO DiffDocsProblem" problem

    DiffBadRegistry problem ->
      error "TODO DiffBadRegistry" problem



-- INSTALL


data Install
  = InstallNoProject
  | InstallNoArgs FilePath
  | InstallNoOnlineSolution
  | InstallNoOfflineSolution
  | InstallHadSolverTrouble Solver
  | InstallUnknownPackageOnline [Pkg.Name]
  | InstallUnknownPackageOffline [Pkg.Name]
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

    InstallUnknownPackageOnline suggestions ->
      error "TODO InstallUnknownPackageOnline" suggestions

    InstallUnknownPackageOffline suggestions ->
      error "TODO InstallUnknownPackageOffline" suggestions

    InstallHasBadDetails _ ->
      error "TODO InstallHasBadDetails"


{- TODO detect if library has no 0.19.0 version. Maybe do this on website though?
  Help.report "OLD DEPENDENCIES" (Just "elm.json")
    ( "The following packages do not work with Elm " ++ V.toChars V.compiler ++ " right now:"
    )
    [ D.indent 4 $ D.vcat $ map (D.red . D.fromUtf8 . Pkg.toString) badPackages
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


data RegistryProblem
  = RP_Http Http.Error
  | RP_Data


data PackageProblem
  = PP_BadEndpointRequest Http.Error
  | PP_BadEndpointContent
  | PP_BadArchiveRequest Http.Error
  | PP_BadArchiveContent
  | PP_BadArchiveHash String String



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


_toReport :: Exit -> Help.Report
_toReport exit =
  case exit of

    -- bump

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
      error "TODO BumpMustHaveLatestRegistry" problem

    BumpCannotFindDocs name version problem ->
      error "TODO BumpCannotFindDocs" name version problem

    -- publish

    PublishMustHaveLatestRegistry problem ->
      error "TODO PublishMustHaveLatestRegistry" problem

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
      badReadmeReport "NO README" $
        "Every published package must have a helpful README.md\
        \ file, but I do not see one in your project."

    PublishShortReadme ->
      badReadmeReport "SHORT README" $
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

    PublishCannotGetTag httpError ->
      error "TODO PublishCannotGetTag" httpError

    PublishCannotGetZip httpError ->
      error "TODO PublishCannotGetZip" httpError

    PublishCannotDecodeZip ->
      error "TODO PublishCannotDecodeZip"

    PublishCannotGetDocs docsProblem ->
      error "TODO PublishCannotGetDocs" docsProblem

    PublishCannotRegister httpError ->
      error "TODO PublishCannotRegister" httpError

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


badReadmeReport :: String -> String -> Help.Report
badReadmeReport title summary =
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

    DetailsBadOutline outlineProblem ->
      toOutlineReport outlineProblem

    DetailsCannotGetRegistry problem ->
      error "TODO DetailsCannotGetRegistry" problem

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
    OutlineHasBadSrcDirs dir dirs ->
      error "TODO OutlineHasBadSrcDirs" dir dirs

    OutlineHasBadStructure decodeError ->
      error "TODO OutlineHasBadStructure" decodeError toOutlineProblemReport


toOutlineProblemReport :: OutlineProblem -> Help.Report
toOutlineProblemReport problem =
  case problem of
    OP_BadType ->
      error "TODO OP_BadType"

    OP_BadPkgName row col ->
      error "TODO OP_BadPkgName" row col

    OP_BadVersion row col ->
      error "TODO OP_BadVersion" row col

    OP_BadConstraint constraintError ->
      error "TODO OP_BadConstraint" constraintError

    OP_BadModuleName row col ->
      error "TODO OP_BadModuleName" row col

    OP_BadModuleHeaderTooLong ->
      error "TODO OP_BadModuleHeaderTooLong"

    OP_BadDependencyName row col ->
      error "TODO OP_BadDependencyName"

    OP_BadLicense badLicense suggestions ->
      error "TODO OP_BadLicense" badLicense suggestions

    OP_BadSummaryTooLong ->
      error "TODO OP_BadSummaryTooLong"

    OP_NoSrcDirs ->
      error "TODO OP_NoSrcDirs"

