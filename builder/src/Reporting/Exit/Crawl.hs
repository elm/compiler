{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit.Crawl
  ( Exit(..)
  , Problem(..)
  , Origin(..)
  , toReport
  )
  where


import qualified Data.ByteString as BS
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Text
import qualified Data.Time.Clock as Time

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import Reporting.Doc ((<+>), (<>))
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Compile as Compile
import qualified Reporting.Exit.Help as Help



-- EXIT


data Exit
  = RootFileNotFound FilePath
  | RootModuleNameDuplicate Module.Raw [FilePath]
  | RootNameless FilePath
  | DependencyProblems Problem [Problem]
  | BadKernelHeader FilePath


data Problem
  = ModuleNotFound Origin Module.Raw [FilePath]
  | ModuleAmbiguous Origin Module.Raw [FilePath] [Pkg.Package]
  | BadHeader FilePath Time.UTCTime BS.ByteString Compiler.Error
  | ModuleNameReservedForKernel Origin Module.Raw
  | ModuleNameMissing FilePath Module.Raw
  | ModuleNameMismatch
      FilePath
      Module.Raw -- expected
      Module.Raw -- actual
  | PortsInPackage FilePath Module.Raw
  | EffectsUnexpected FilePath Module.Raw


data Origin
  = ElmJson
  | File FilePath
  | Module FilePath Module.Raw



-- TO REPORT


toReport :: Exit -> Help.Report
toReport exit =
  case exit of
    RootFileNotFound path ->
      Help.report "FILE NOT FOUND" Nothing
        "You want me to compile this file:"
        [ D.indent 4 $ D.dullyellow $ D.fromString path
        , "I cannot find it though! Is there a typo?"
        ]

    RootModuleNameDuplicate name paths ->
      Help.report "DUPLICATE NAMES" Nothing
        "I am trying to compile multiple modules with the same name:"
        [ D.indent 4 $ D.dullyellow $ D.vcat $
            map D.fromString paths
        , D.fillSep $
            [ "These", "modules", if length paths == 2 then "both" else "all", "claim"
            , "to", "be", "named", D.dullyellow (D.fromString (Module.nameToString name)) <> "."
            , "Change", "them", "to", "have", "unique", "names", "and", "you"
            , "should", "be", "all", "set!"
            ]
        ]

    RootNameless path ->
      namelessToDoc path "Main"

    DependencyProblems problem otherProblems ->
      case otherProblems of
        [] ->
          problemToReport problem

        _ ->
          problemToReport problem
          -- error "TODO handle multiple dependency errors"

    BadKernelHeader filePath ->
      Help.report "BAD KERNEL HEADER" Nothing
        "I ran into a bad header in this file:"
        [ D.indent 4 $ D.dullyellow $ D.fromString filePath
        , D.reflow $
            "NOTE: Kernel code is only available to core Elm libraries to ensure\
            \ the portability and security of the Elm ecosystem. This restriction\
            \ also makes it possible to improve code gen (i.e. performance) without\
            \ disrupting the ecosystem."
        ]



-- PROBLEM TO REPORT


problemToReport :: Problem -> Help.Report
problemToReport problem =
  case problem of
    ModuleNotFound origin name dirs ->
      notFoundToDoc origin name dirs

    ModuleAmbiguous origin child paths pkgs ->
      ambiguousToDoc origin child paths pkgs

    BadHeader path time source compilerError ->
      let
        compilerExit =
          Compile.Exit "???" path time (Text.decodeUtf8 source) [compilerError]
      in
      Help.compilerReport compilerExit []

    ModuleNameMissing path name ->
      namelessToDoc path name

    ModuleNameMismatch path expected actual ->
      Help.report "MODULE NAME MISMATCH" (Just path)
        ( "The file at " ++ path ++ " has a typo in the module name. It says:"
        )
        [ D.indent 4 $ D.dullyellow $ "module" <+> D.red (D.fromString (Module.nameToString actual)) <+> "exposing (..)"
        , "Looks like a typo or copy/paste error. Instead it needs to say:"
        , D.indent 4 $ D.dullyellow $ "module" <+> D.green (D.fromString (Module.nameToString expected)) <+> "exposing (..)"
        , "Make the change and you should be all set!"
        ]

    ModuleNameReservedForKernel origin name ->
      kernelNameToDoc origin name

    PortsInPackage path name ->
      badTagToDoc path name "port" "port-modules" $
        "Packages cannot have any `port` modules."

    EffectsUnexpected path name ->
      badTagToDoc path name "effect" "effect-modules" $
        "Creating `effect` modules is relatively experimental. There are a\
        \ couple in @elm repos right now, but we have decided to be very\
        \ cautious in expanding its usage."


badTagToDoc :: FilePath -> Module.Raw -> String -> String -> String -> Help.Report
badTagToDoc path name tag hintName summary =
  Help.report
    ("UNEXPECTED " ++ map Char.toUpper tag ++ " MODULE")
    (Just path)
    summary
    [ D.fillSep $
        [ "Get", "rid", "of", "all", "the"
        , D.red (D.fromString tag)
        , "stuff", "in"
        , D.dullyellow (D.fromString (Module.nameToString name))
        , "to", "proceed."
        ]
    , D.toSimpleNote $
        "You can learn the reasoning behind this design choice at "
        ++ D.makeLink hintName
    ]



-- HELPERS


namelessToDoc :: FilePath -> Module.Raw -> Help.Report
namelessToDoc path name =
  Help.report "UNNAMED MODULE" (Just path)
    ( "The `" ++ Module.nameToString name
      ++ "` module must start with a line like this:"
    )
    [ D.indent 4 $ D.dullyellow $ D.fromString $
        "module " ++ Module.nameToString name ++ " exposing (..)"
    , D.reflow $
        "Try adding that as the first line of your file!"
    , D.toSimpleNote $
        "It is best to replace (..) with an explicit list of types and\
        \ functions you want to expose. If you know a value is only used\
        \ WITHIN this module, it is extra easy to refactor. This kind of\
        \ information is great, especially as your project grows!"
    ]


notFoundToDoc :: Origin -> Module.Raw -> [FilePath] -> Help.Report
notFoundToDoc origin child dirs =
  case origin of
    ElmJson ->
      Help.report "MODULE NOT FOUND" (Just "elm.json")
        "Your elm.json says your project has the following module:"
        [ D.indent 4 $ D.red $ D.fromString $ Module.nameToString child
        , D.reflow $
            "When creating packages, all modules must live in the\
            \ src/ directory, but I cannot find it there."
        ]

    File path ->
      Help.report "UNKNOWN IMPORT" (Just path)
        ("I cannot find a `" ++ Module.nameToString child ++ "` module to import.")
        (notFoundDetails child dirs)

    Module path parent ->
      Help.report "UNKNOWN IMPORT" (Just path)
        ("The " ++ Module.nameToString parent ++ " module has a bad import:")
        (notFoundDetails child dirs)


notFoundDetails :: Module.Raw -> [FilePath] -> [D.Doc]
notFoundDetails child dirs =
  let
    simulatedCode =
      D.indent 4 $ D.red $ D.fromString $ "import " ++ Module.nameToString child
  in
  case Map.lookup child Pkg.suggestions of
    Just pkg ->
      [ simulatedCode
      , D.reflow $
          "Do you want the one from the " ++ Pkg.toString pkg
          ++ " package? If so, run this command to add that dependency to your elm.json file:"
      , D.indent 4 $ D.green $ D.fromString $ "elm install " ++ Pkg.toString pkg
      , D.reflow $
          "If you want a local file, make sure the directory that contains the `"
          ++ Module.nameToString child
          ++ "` module is listed in your elm.json \"source-directories\" field."
      ]

    Nothing ->
      [ simulatedCode
      , "I cannot find that module! Is there a typo in the module name?"
      , D.reflow $
          case dirs of
            [] ->
              "The \"source-directories\" field of your elm.json is empty, so I only\
              \ searched in packages. Maybe you need to add a \"src\" directory there?"

            [dir] ->
              "The \"source-directories\" field of your elm.json tells me to only look in the "
              ++ dir ++ " directory, but it is not there. Maybe it is in a package that is not installed yet?"

            dir:_:_ ->
              "The \"source-directories\" field of your elm.json tells me to look in directories like "
              ++ dir ++ ", but it is not in any of them. Maybe it is in a package that is not installed yet?"
      ]


ambiguousToDoc :: Origin -> Module.Raw -> [FilePath] -> [Pkg.Package] -> Help.Report
ambiguousToDoc origin child paths pkgs =
  let
    pkgToString (Pkg.Package pkg vsn) =
      "exposed by " ++ Pkg.toString pkg ++ " " ++ Pkg.versionToString vsn

    makeReport maybePath summary yellowString =
      Help.report "AMBIGUOUS IMPORT" maybePath summary
        [ D.indent 4 $ D.dullyellow $ D.fromString yellowString
        , D.reflow $
            "I found multiple module with that name though:"
        , D.indent 4 $ D.dullyellow $ D.vcat $
            map D.fromString $ paths ++ map pkgToString pkgs
        , D.reflow $
            if null paths then
              "It looks like the name clash is in your dependencies, which is\
              \ out of your control. Elm does not support this scenario right\
              \ now, but it may be worthwhile. Please open an issue describing\
              \ your scenario so we can gather more usage information!"
            else
              "Which is the right one? Try renaming your modules to have unique names."
        ]
  in
    case origin of
      ElmJson ->
        makeReport
          Nothing
          "Your elm.json wants the following module:"
          (Module.nameToString child)

      File path ->
        makeReport
          (Just path)
          ("The file at " ++ path ++ " has an ambiguous import:")
          ("import " ++ Module.nameToString child)

      Module path parent ->
        makeReport
          (Just path)
          ("The " ++ Module.nameToString parent ++ " module has an ambiguous import:")
          ("import " ++ Module.nameToString child)


kernelNameToDoc :: Origin -> Module.Raw -> Help.Report
kernelNameToDoc origin kernelName =
  let
    (maybePath, statement) =
      case origin of
        ElmJson ->
          ( Nothing
          , "Your elm.json says your project has the following module:"
          )

        File path ->
          ( Just path
          , "This file is trying to import the following module:"
          )

        Module path parent ->
          ( Just path
          , "Your " ++ Module.nameToString parent ++ " module is trying to import:"
          )
  in
  Help.report "BAD MODULE NAME" maybePath statement
    [ D.indent 4 $ D.dullyellow $ D.fromString $ Module.nameToString kernelName
    , D.reflow $
        "But names like that are reserved for internal use.\
        \ Switch to a name outside of the Elm/Kernel/ namespace."
    ]
