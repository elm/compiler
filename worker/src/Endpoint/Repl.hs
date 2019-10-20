{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Endpoint.Repl
  ( endpoint
  )
  where


import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.Name as N
import qualified Data.NonEmptyList as NE
import Snap.Core

import qualified Artifacts as A
import qualified Cors

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified Compile
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified File
import qualified Generate.JavaScript as JS
import qualified Json.Encode as Encode
import qualified Parse.Module as Parse
import qualified Repl
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error
import qualified Reporting.Error.Import as Import
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Render.Type.Localizer as L



-- ALLOWED ORIGINS


allowedOrigins :: [String]
allowedOrigins =
  [ "https://guide.elm-lang.org"
  , "https://guide.elm-lang.jp"
  , "http://localhost:8007"
  ]



-- ENDPOINT


endpoint :: A.Artifacts -> Snap ()
endpoint artifacts =
  Cors.allow POST allowedOrigins $
  do  body <- readRequestBody (64 * 1024)
      case decodeBody body of
        Just (state, entry) ->
          serveOutcome (toOutcome artifacts state entry)

        Nothing ->
          do  modifyResponse $ setResponseStatus 400 "Bad Request"
              modifyResponse $ setContentType "text/html; charset=utf-8"
              writeBS "Received unexpected JSON body."



-- TO OUTCOME


data Outcome
  = NewImport N.Name
  | NewType N.Name
  | NewWork B.Builder
  --
  | Skip
  | Indent
  | DefStart N.Name
  --
  | NoPorts
  | InvalidCommand
  | Failure BS.ByteString Error.Error


toOutcome :: A.Artifacts -> Repl.State -> String -> Outcome
toOutcome artifacts state entry =
  case reverse (lines entry) of
    [] ->
      Skip

    prev : rev ->
      case Repl.categorize (Repl.Lines prev rev) of
        Repl.Done input ->
          case input of
            Repl.Import name src -> compile artifacts state (ImportEntry name src)
            Repl.Type name src   -> compile artifacts state (TypeEntry name src)
            Repl.Decl name src   -> compile artifacts state (DeclEntry name src)
            Repl.Expr src        -> compile artifacts state (ExprEntry src)
            Repl.Port            -> NoPorts
            Repl.Skip            -> Skip
            Repl.Reset           -> InvalidCommand
            Repl.Exit            -> InvalidCommand
            Repl.Help _          -> InvalidCommand

        Repl.Continue prefill ->
          case prefill of
            Repl.Indent        -> Indent
            Repl.DefStart name -> DefStart name



-- SERVE OUTCOME


serveOutcome :: Outcome -> Snap ()
serveOutcome outcome =
  let
    serveString = serveBuilder "text/plain"
  in
  case outcome of
    NewImport name -> serveString $ "add-import:" <> N.toBuilder name
    NewType name   -> serveString $ "add-type:" <> N.toBuilder name
    NewWork js     -> serveBuilder "application/javascript" js
    Skip           -> serveString $ "skip"
    Indent         -> serveString $ "indent"
    DefStart name  -> serveString $ "def-start:" <> N.toBuilder name
    NoPorts        -> serveString $ "no-ports"
    InvalidCommand -> serveString $ "invalid-command"
    Failure source err ->
      serveBuilder "application/json" $ Encode.encodeUgly $ Exit.toJson $
        Help.compilerReport "/" (Error.Module N.replModule "/repl" File.zeroTime source err) []


serveBuilder :: BS.ByteString -> B.Builder -> Snap ()
serveBuilder mime builder =
  do  modifyResponse (setContentType mime)
      writeBuilder builder



-- COMPILE


data EntryType
  = ImportEntry N.Name BS.ByteString
  | TypeEntry N.Name BS.ByteString
  | DeclEntry N.Name BS.ByteString
  | ExprEntry BS.ByteString


compile :: A.Artifacts -> Repl.State -> EntryType -> Outcome
compile (A.Artifacts interfaces objects) state@(Repl.State imports types decls) entryType =
  let
    source =
      case entryType of
        ImportEntry name src -> Repl.toByteString (state { Repl._imports = Map.insert name (B.byteString src) imports }) Repl.OutputNothing
        TypeEntry   name src -> Repl.toByteString (state { Repl._types = Map.insert name (B.byteString src) types }) Repl.OutputNothing
        DeclEntry   name src -> Repl.toByteString (state { Repl._decls = Map.insert name (B.byteString src) decls }) (Repl.OutputDecl name)
        ExprEntry        src -> Repl.toByteString state (Repl.OutputExpr src)
  in
  case
    do  modul <- mapLeft Error.BadSyntax $ Parse.fromByteString Parse.Application source
        ifaces <- mapLeft Error.BadImports $ checkImports interfaces (Src._imports modul)
        artifacts <- Compile.compile Pkg.dummyName ifaces modul
        return ( modul, artifacts, objects )
  of
    Left err ->
      Failure source err

    Right info ->
      case entryType of
        ImportEntry name _ -> NewImport name
        TypeEntry name _   -> NewType name
        DeclEntry name _   -> NewWork (toJavaScript info (Just name))
        ExprEntry _        -> NewWork (toJavaScript info Nothing)


toJavaScript :: (Src.Module, Compile.Artifacts, Opt.GlobalGraph) -> Maybe N.Name -> B.Builder
toJavaScript (modul, Compile.Artifacts canModule types locals, objects) maybeName =
  let
    localizer = L.fromModule modul
    graph = Opt.addLocalGraph locals objects
    home = Can._name canModule
    tipe = types ! maybe N.replValueToPrint id maybeName
  in
  JS.generateForReplEndpoint localizer graph home maybeName tipe


mapLeft :: (x -> y) -> Either x a -> Either y a
mapLeft func result =
  either (Left . func) Right result


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



-- DECODE BODY


decodeBody :: LBS.ByteString -> Maybe ( Repl.State, String )
decodeBody body =
  Aeson.parseMaybe decodeBodyHelp =<< Aeson.decode' body


decodeBodyHelp :: Aeson.Object -> Aeson.Parser ( Repl.State, String )
decodeBodyHelp obj =
  let
    get key =
      do  dict <- obj .: key
          let f (k,v) = (N.fromChars k, B.stringUtf8 v)
          return $ Map.fromList $ map f $ Map.toList dict
  in
  do  imports <- get "imports"
      types   <- get "types"
      decls   <- get "decls"
      entry   <- obj .: "entry"
      return ( Repl.State imports types decls, entry )
