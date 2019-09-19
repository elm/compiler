{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Endpoint.Repl
  ( endpoint
  )
  where


import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BS_UTF8
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.Name as N
import qualified Data.NonEmptyList as NE
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr)
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
import qualified Json.Decode as D
import qualified Parse.Module as Parse
import qualified Parse.Primitives as P
import qualified Parse.Variable as Var
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
  [ "https://elm-lang.org"
  , "https://guide.elm-lang.org"
  ]



-- ENDPOINT


endpoint :: A.Artifacts -> Snap ()
endpoint artifacts =
  Cors.allow POST allowedOrigins $
  do  body <- LBS.toStrict <$> readRequestBody (64 * 1024)
      case D.fromByteString decoder body of
        Right (state, entry) ->
          serveOutcome (toOutcome artifacts state entry)

        Left _ ->
          do  modifyResponse $ setResponseStatus 400 "Bad Request"
              modifyResponse $ setContentType "text/html; charset=utf-8"
              writeBS "Received unexpected JSON body."



-- TO OUTCOME


data Outcome
  = NewImport N.Name
  | NewType N.Name
  | NewWork B.Builder
  --
  | Reset
  | Skip
  | Indent
  | DefStart N.Name
  --
  | NoPorts
  | InvalidCommand
  | Failure BS.ByteString Error.Error


toOutcome :: A.Artifacts -> Repl.State -> BS.ByteString -> Outcome
toOutcome artifacts state entry =
  case reverse (lines (BS_UTF8.toString entry)) of
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
            Repl.Reset           -> Reset
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
    Reset          -> serveString $ "reset"
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
compile (A.Artifacts interfaces objects) state entryType =
  let
    source =
      Repl.toByteString state $
        case entryType of
          ImportEntry _ _  -> Repl.OutputNothing
          TypeEntry _ _    -> Repl.OutputNothing
          DeclEntry name _ -> Repl.OutputDecl name
          ExprEntry src    -> Repl.OutputExpr src
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



-- DECODER


decoder :: D.Decoder () ( Repl.State, BS.ByteString )
decoder =
  let
    bail _ _ = ()
    upper = D.KeyDecoder (Var.upper bail) bail
    lower = D.KeyDecoder (Var.lower bail) bail
    bytes = D.customString bytesParser bail
    builder = B.byteString <$> bytes
  in
  do  imports <- D.field "imports" (D.dict upper builder)
      types   <- D.field "types" (D.dict upper builder)
      decls   <- D.field "decls" (D.dict lower builder)
      entry   <- D.field "entry" bytes
      return ( Repl.State imports types decls, entry )


bytesParser :: P.Parser x BS.ByteString
bytesParser =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ _ ->
    let
      off = minusPtr pos (unsafeForeignPtrToPtr src)
      len = minusPtr end pos
      newCol = col + fromIntegral len
    in
    cok (BS.PS src off len) (P.State src end end indent row newCol)
