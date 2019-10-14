{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Import
  ( Error(..)
  , Problem(..)
  , toReport
  )
  where


import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Reporting.Doc as D
import qualified Reporting.Render.Code as Code
import qualified Reporting.Report as Report
import qualified Reporting.Suggest as Suggest
import qualified Reporting.Annotation as A



-- ERROR


data Error =
  Error
    { _region :: A.Region
    , _import :: ModuleName.Raw
    , _unimported :: Set.Set ModuleName.Raw
    , _problem :: Problem
    }


data Problem
  = NotFound
  | Ambiguous FilePath [FilePath] Pkg.Name [Pkg.Name]
  | AmbiguousLocal FilePath FilePath [FilePath]
  | AmbiguousForeign Pkg.Name Pkg.Name [Pkg.Name]



-- TO REPORT


toReport :: Code.Source -> Error -> Report.Report
toReport source (Error region name unimportedModules problem) =
  case problem of
    NotFound ->
      Report.Report "MODULE NOT FOUND" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "You are trying to import a `" ++ ModuleName.toChars name ++ "` module:"
          ,
            D.stack
              [
                D.reflow $
                  "I checked the \"dependencies\" and \"source-directories\" listed in your elm.json,\
                  \ but I cannot find it! Maybe it is a typo for one of these names?"
              ,
                D.dullyellow $ D.indent 4 $ D.vcat $
                  map D.fromName (toSuggestions name unimportedModules)
              ,
                case Map.lookup name Pkg.suggestions of
                  Nothing ->
                    D.toSimpleHint $
                      "If it is not a typo, check the \"dependencies\" and \"source-directories\"\
                      \ of your elm.json to make sure all the packages you need are listed there!"

                  Just dependency ->
                    D.toFancyHint
                      ["Maybe","you","want","the"
                      ,"`" <> D.fromName name <> "`"
                      ,"module","defined","in","the"
                      ,D.fromChars (Pkg.toChars dependency)
                      ,"package?","Running"
                      ,D.green (D.fromChars ("elm install " ++ Pkg.toChars dependency))
                      ,"should","make","it","available!"
                      ]
              ]
          )

    Ambiguous path _ pkg _ ->
      Report.Report "AMBIGUOUS IMPORT" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "You are trying to import a `" ++ ModuleName.toChars name ++ "` module:"
          ,
            D.stack
              [
                D.fillSep $
                  ["But","I","found","multiple","modules","with","that","name.","One","in","the"
                  ,D.dullyellow (D.fromChars (Pkg.toChars pkg))
                  ,"package,","and","another","defined","locally","in","the"
                  ,D.dullyellow (D.fromChars path)
                  ,"file.","I","do","not","have","a","way","to","choose","between","them."
                  ]
              ,
                D.reflow $
                  "Try changing the name of the locally defined module to clear up the ambiguity?"
              ]
          )

    AmbiguousLocal path1 path2 paths ->
      Report.Report "AMBIGUOUS IMPORT" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "You are trying to import a `" ++ ModuleName.toChars name ++ "` module:"
          ,
            D.stack
              [
                D.reflow $
                  "But I found multiple files in your \"source-directories\" with that name:"
              ,
                D.dullyellow $ D.indent 4 $ D.vcat $
                  map D.fromChars (path1:path2:paths)
              ,
                D.reflow $
                  "Change the module names to be distinct!"
              ]
          )

    AmbiguousForeign pkg1 pkg2 pkgs ->
      Report.Report "AMBIGUOUS IMPORT" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "You are trying to import a `" ++ ModuleName.toChars name ++ "` module:"
          ,
            D.stack
              [
                D.reflow $
                  "But multiple packages in your \"dependencies\" that expose a module that name:"
              ,
                D.dullyellow $ D.indent 4 $ D.vcat $
                  map (D.fromChars . Pkg.toChars) (pkg1:pkg2:pkgs)
              ,
                D.reflow $
                  "There is no way to disambiguate in cases like this right now. Of the known name\
                  \ clashes, they are usually for packages with similar purposes, so the current\
                  \ recommendation is to pick just one of them."
              , D.toSimpleNote $
                  "It seems possible to resolve this with new syntax in imports, but that is\
                  \ more complicated than it sounds. Right now, our module names are tied to GitHub\
                  \ repos, but we may want to get rid of that dependency for a variety of reasons.\
                  \ That would in turn have implications for our package infrastructure, hosting\
                  \ costs, and possibly on how package names are specified. The particular syntax\
                  \ chosen seems like it would interact with all these factors in ways that are\
                  \ difficult to predict, potentially leading to harder problems later on. So more\
                  \ design work and planning is needed on these topics."
              ]
          )



toSuggestions :: ModuleName.Raw -> Set.Set ModuleName.Raw -> [ModuleName.Raw]
toSuggestions name unimportedModules =
  take 4 $
    Suggest.sort (ModuleName.toChars name) ModuleName.toChars (Set.toList unimportedModules)
