{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit.Publish
  ( Exit(..)
  , toReport
  )
  where


import Deps.Diff (Magnitude, magnitudeToString)
import qualified Elm.Package as Pkg
import Reporting.Doc ((<>))
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help



-- EXITS


data Exit
  = Application
  | NotInitialVersion Pkg.Version
  | AlreadyPublished Pkg.Version
  | InvalidBump Pkg.Version Pkg.Version
  | BadBump Pkg.Version Pkg.Version Magnitude Pkg.Version Magnitude
  | NoSummary
  | NoExposed
  | NoReadme
  | ShortReadme
  | NoLicense
  | MissingTag Pkg.Version
  | NoGit
  | LocalChanges Pkg.Version



-- TO REPORT


toReport :: Exit -> Help.Report
toReport exit =
  case exit of
    Application ->
      Help.report "UNPUBLISHABLE" Nothing "I cannot publish applications, only packages!" []

    NotInitialVersion vsn ->
      Help.docReport "INVALID VERSION" Nothing
        ( D.fillSep
            ["I","cannot","publish"
            ,D.red (D.fromString (Pkg.versionToString vsn))
            ,"as","the","initial","version."
            ]
        )
        [ D.fillSep
            ["Change","it","to",D.green "1.0.0","which","is"
            ,"the","initial","version","for","all","Elm","packages."
            ]
        ]

    AlreadyPublished vsn ->
      Help.docReport "ALREADY PUBLISHED" Nothing
        ( D.vcat
            [ D.fillSep
                [ "Version", D.green (D.fromString (Pkg.versionToString vsn))
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

    InvalidBump statedVersion latestVersion ->
      Help.docReport "INVALID VERSION" (Just "elm.json")
        ( D.fillSep $
            ["Your","elm.json","says","the","next","version","should","be"
            ,D.red (D.fromString (Pkg.versionToString statedVersion)) <> ","
            ,"but","that","is","not","valid","based","on","the","previously"
            ,"published","versions."
            ]
        )
        [ D.fillSep $
            ["Change","the","version","back","to"
            ,D.green (D.fromString (Pkg.versionToString latestVersion))
            ,"which","is","the","most","recently","published","version."
            ,"From","there,","have","Elm","bump","the","version","by","running:"
            ]
        , D.indent 4 $ D.green "elm bump"
        , D.reflow $
            "If you want more insight on the API changes Elm detects, you\
            \ can run `elm diff` at this point as well."
        ]

    BadBump old new magnitude realNew realMagnitude ->
      Help.docReport "INVALID VERSION" (Just "elm.json")
        (
          D.fillSep $
            ["Your","elm.json","says","the","next","version","should","be"
            ,D.red (D.fromString (Pkg.versionToString new)) <> ","
            ,"indicating","a",D.fromString (magnitudeToString magnitude)
            ,"change","to","the","public","API."
            ,"This","does","not","match","the","API","diff","given","by:"
            ]
        )
        [ D.indent 4 $ D.fromString $
            "elm diff " ++ Pkg.versionToString old

        , D.fillSep $
            ["This","command","says","this","is","a"
            ,D.fromString (magnitudeToString realMagnitude)
            ,"change,","so","the","next","version","should","be"
            ,D.green (D.fromString (Pkg.versionToString realNew)) <> "."
            ,"Double","check","everything","to","make","sure","you"
            ,"are","publishing","what","you","want!"
            ]
        , D.reflow $
            "Also, next time use `elm bump` and I'll figure all this out for you!"
        ]

    NoSummary ->
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

    NoExposed ->
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

    NoReadme ->
      badReadmeReport "NO README" $
        "Every published package must have a helpful README.md\
        \ file, but I do not see one in your project."

    ShortReadme ->
      badReadmeReport "SHORT README" $
        "This README.md is too short. Having more details will help\
        \ people assess your package quickly and fairly."

    NoLicense ->
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

    MissingTag version ->
      let vsn = Pkg.versionToString version in
      Help.docReport "NO TAG" Nothing
        ( D.fillSep $
            [ "Packages", "must", "be", "tagged", "in", "git,", "but", "I"
            , "cannot", "find", "a", D.green (D.fromString vsn), "tag."
            ]
        )
        [ D.vcat
            [ "These tags make it possible to find this specific version on GitHub."
            , "To tag the most recent commit and push it to GitHub, run this:"
            ]
        , D.indent 4 $ D.dullyellow $ D.vcat $ map D.fromString $
            [ "git tag -a " ++ vsn ++ " -m \"new release\""
            , "git push origin " ++ vsn
            ]
        , "The -m flag is for a helpful message. Try to make it more informative!"
        ]

    NoGit ->
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

    LocalChanges version ->
      let vsn = Pkg.versionToString version in
      Help.docReport "LOCAL CHANGES" Nothing
        ( D.fillSep $
            [ "The", "code", "tagged", "as", D.green (D.fromString vsn), "in"
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
