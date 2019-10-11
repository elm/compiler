{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Endpoint.Compile
  ( endpoint
  , loadErrorJS
  )
  where


import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.Name as N
import qualified Data.NonEmptyList as NE
import Snap.Core
import Snap.Util.FileUploads
import qualified System.Directory as Dir
import qualified System.IO.Streams as Stream
import Text.RawString.QQ (r)

import qualified Artifacts as A
import qualified Cors

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified BackgroundWriter as BW
import qualified Build
import qualified Compile
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified File
import qualified Generate
import qualified Generate.Html as Html
import qualified Generate.JavaScript as JS
import qualified Generate.Mode as Mode
import qualified Json.Encode as Encode
import qualified Parse.Module as Parse
import qualified Reporting
import qualified Reporting.Annotation as A
import Reporting.Doc ((<>))
import qualified Reporting.Doc as D
import qualified Reporting.Error as Error
import qualified Reporting.Error.Import as Import
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Task as Task



-- ALLOWED ORIGINS


allowedOrigins :: [String]
allowedOrigins =
  [ "https://elm-lang.org"
  , "https://package.elm-lang.org"
  ]



-- ENDPOINT


endpoint :: A.Artifacts -> Snap ()
endpoint artifacts =
  Cors.allow POST allowedOrigins $
  do  result <- foldMultipart defaultUploadPolicy ignoreFile 0
      case result of
        ([("code",source)], 0) ->
          do  modifyResponse $ setContentType "text/html; charset=utf-8"
              case compile artifacts source of
                Success builder ->
                  writeBuilder builder

                NoMain ->
                  writeBuilder $ renderReport noMain

                BadInput name err ->
                  writeBuilder $ renderReport $
                    Help.compilerReport "/" (Error.Module name "/try" File.zeroTime source err) []

        _ ->
          do  modifyResponse $ setResponseStatus 400 "Bad Request"
              modifyResponse $ setContentType "text/html; charset=utf-8"
              writeBS
                "<p>Unexpected request format. This should not be possible!</p>\
                \<p>Please report this\
                \ <a href=\"https://github.com/elm/compiler/issues\">here</a>\
                \ along with the URL and your browser version.</p>"


ignoreFile :: PartInfo -> Stream.InputStream B.ByteString -> Int -> IO Int
ignoreFile _ _ count =
  return (count + 1)



-- COMPILE


data Outcome
  = Success B.Builder
  | NoMain
  | BadInput ModuleName.Raw Error.Error


compile :: A.Artifacts -> B.ByteString -> Outcome
compile (A.Artifacts interfaces objects) source =
  case Parse.fromByteString Parse.Application source of
    Left err ->
      BadInput N._Main (Error.BadSyntax err)

    Right modul@(Src.Module _ _ _ imports _ _ _ _ _) ->
      case checkImports interfaces imports of
        Left err ->
          BadInput (Src.getName modul) (Error.BadImports err)

        Right ifaces ->
          case Compile.compile Pkg.dummyName ifaces modul of
            Left err ->
              BadInput (Src.getName modul) err

            Right (Compile.Artifacts canModule _ locals) ->
              case locals of
                Opt.LocalGraph Nothing _ _ ->
                  NoMain

                Opt.LocalGraph (Just main_) _ _ ->
                  let
                    mode  = Mode.Dev Nothing
                    home  = Can._name canModule
                    name  = ModuleName._module home
                    mains = Map.singleton home main_
                    graph = Opt.addLocalGraph locals objects
                  in
                  Success $ Html.sandwich name $ JS.generate mode graph mains


checkImports :: Map.Map ModuleName.Raw I.Interface -> [Src.Import] -> Either (NE.List Import.Error) (Map.Map ModuleName.Raw I.Interface)
checkImports interfaces imports =
  let
    importDict = Map.fromValues Src.getImportName imports
    missing = Map.difference importDict interfaces
  in
  case Map.elems missing of
    [] ->
      Right (Map.intersection interfaces importDict)

    i:is ->
      let
        unimported =
          Map.keysSet (Map.difference interfaces importDict)

        toError (Src.Import (A.At region name) _ _) =
          Import.Error region name unimported Import.NotFound
      in
      Left (fmap toError (NE.List i is))



-- RENDER REPORT


renderReport :: Help.Report -> B.Builder
renderReport report =
  [r|<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <style>body { padding: 0; margin: 0; background-color: black; }</style>
  <script src="https://worker.elm-lang.org/compile/errors.js"></script>
</head>
<body>
  <script>
    var app = Elm.Errors.init({flags:|] <> Encode.encodeUgly (Exit.toJson report) <> [r|});
    app.ports.jumpTo.subscribe(function(region) {
      window.parent.postMessage(JSON.stringify(region), '*');
    });
  </script>
</body>
</html>|]



-- NO MAIN


noMain :: Help.Report
noMain =
  Help.report "NO MAIN" Nothing
    (
      "Without a `main` value, I do not know what to show on screen!"
    )
    [ D.reflow $
        "Adding a `main` value can be as brief as:"
    , D.vcat
        [ D.fillSep [D.cyan "import","Html"]
        , ""
        , D.fillSep [D.green "main","="]
        , D.indent 2 $ D.fillSep [D.cyan "Html" <> ".text",D.dullyellow "\"Hello!\""]
        ]
    , D.reflow $
        "Try adding something like that!"
    , D.toSimpleNote $
        "I recommend looking through https://guide.elm-lang.org for more advice on\
        \ how to fill in `main` values."
    ]



-- LOAD ERROR JS


loadErrorJS :: IO B.ByteString
loadErrorJS =
  let
    run work =
      do  result <- work
          case result of
            Right a -> return a
            Left _ -> error "problem building src/Errors.elm"
  in
  BW.withScope $ \scope ->
    do  root <- Dir.getCurrentDirectory
        details <- run $ Details.load Reporting.silent scope root
        artifacts <- run $ Build.fromPaths Reporting.silent root details (NE.List "src/Errors.elm" [])
        javascript <- run $ Task.run $ Generate.prod root details artifacts
        return $ LBS.toStrict $ B.toLazyByteString javascript
