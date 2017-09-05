{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler
  ( version
  , compile, Context(..), Result(..)
  , generate
  , Localizer, dummyLocalizer
  , Error, errorToDoc, errorToJson
  , Warning, warningToDoc, warningToJson
  , Tag(..), parseHeader
  , KernelInfo(..), parseKernel
  )
  where

import qualified Data.Aeson as Json
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.ByteString.Builder as BS
import Text.PrettyPrint.ANSI.Leijen (Doc)

import qualified AST.Kernel as Kernel (Info)
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified Compile
import qualified Docs.Check as Docs
import qualified Elm.Compiler.Imports as Imports
import qualified Elm.Compiler.Module as M
import qualified Elm.Compiler.Objects.Internal as Obj
import qualified Elm.Compiler.Version
import qualified Elm.Docs as Docs
import qualified Elm.Package as Package
import qualified Generate.JavaScript as JS
import qualified Parse.Helpers as Parse (run)
import qualified Parse.Kernel as Kernel (parser)
import qualified Parse.Module as Parse (header)
import qualified Reporting.Annotation as A
import qualified Reporting.Bag as Bag
import qualified Reporting.Error as Error
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Report as Report
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- VERSION


version :: Package.Version
version =
  Elm.Compiler.Version.version



-- COMPILATION


{-| Compiles Elm source code to JavaScript. -}
compile :: Context -> Text -> (Localizer, [Warning], Either [Error] Result)
compile (Context packageName exposed importDict interfaces) source =
  let
    (Result.Result oneLocalizer warnings answer) =
      do  modul <- Compile.compile packageName importDict interfaces source
          docs <- Result.format id (docsGen exposed modul)

          let iface = Module.toInterface modul
          let objs = Obj.fromModule modul

          return (Result docs iface objs)
  in
    ( Result.oneToValue dummyLocalizer Localizer oneLocalizer
    , Bag.toList Warning warnings
    , Result.answerToEither Error id answer
    )


data Context =
  Context
    { _package :: Package.Name
    , _exposed :: Bool
    , _imports :: Map.Map M.Raw M.Canonical
    , _interfaces :: M.Interfaces
    }


data Result =
  Result
    { _docs :: Maybe Docs.Module
    , _iface :: M.Interface
    , _objs :: Obj.Graph
    }



-- DOCUMENTATION


docsGen :: Bool -> Module.Optimized -> Result.Result () w Error.Error (Maybe Docs.Module)
docsGen isExposed (Module.Module name info) =
  if not isExposed then
    Result.ok Nothing

  else
    let
      toDocs checked =
        Just (Docs.Module (ModuleName._module name) checked)
    in
      toDocs <$> Docs.check (Module.exports info) (Module.docs info)



-- CODE GENERATION


generate :: Obj.Graph -> Obj.Roots -> BS.Builder
generate =
  JS.generate



-- DEALIASER


newtype Localizer =
    Localizer RenderType.Localizer


dummyLocalizer :: Localizer
dummyLocalizer =
    Localizer Map.empty



-- ERRORS


newtype Error =
    Error (A.Located Error.Error)


errorToDoc :: Localizer -> String -> Text -> Error -> Doc
errorToDoc (Localizer localizer) location source (Error (A.A region err)) =
    Report.toDoc location region (Error.toReport localizer err) source


errorToJson :: Localizer -> String -> Error -> Json.Value
errorToJson (Localizer localizer) location (Error err) =
    Error.toJson localizer location err



-- WARNINGS


newtype Warning =
    Warning (A.Located Warning.Warning)


warningToDoc :: Localizer -> String -> Text -> Warning -> Doc
warningToDoc (Localizer localizer) location source (Warning (A.A region wrn)) =
    Report.toDoc location region (Warning.toReport localizer wrn) source


warningToJson :: Localizer -> String -> Warning -> Json.Value
warningToJson (Localizer localizer) location (Warning wrn) =
    Warning.toJson localizer location wrn



-- PARSE HEADER


data Tag = Normal | Effect | Port


parseHeader :: Package.Name -> Text -> Either Error (Maybe (Tag, M.Raw), [M.Raw])
parseHeader pkgName sourceCode =
  case Parse.run Parse.header sourceCode of
    Right header ->
      Right $ toHeaderSummary pkgName header

    Left err ->
      Left (Error (A.map Error.Syntax err))


toHeaderSummary :: Package.Name -> Module.Header [Module.UserImport] -> (Maybe (Tag, M.Raw), [M.Raw])
toHeaderSummary pkgName (Module.Header maybeHeaderDecl imports) =
  let
    dependencies =
      if pkgName == Package.core
        then map (A.drop . fst . A.drop) imports
        else map (A.drop . fst . A.drop) imports ++ map fst Imports.defaults
  in
    case maybeHeaderDecl of
      Nothing ->
        ( Nothing, dependencies )

      Just (Module.HeaderDecl sourceTag name _ _ _) ->
        let
          tag =
            case sourceTag of
              Module.Normal -> Normal
              Module.Port _ -> Port
              Module.Effect _ -> Effect
        in
          ( Just (tag, name), dependencies )



-- PARSE KERNEL


newtype KernelInfo =
  KernelInfo Kernel.Info


parseKernel :: Text -> Either Error KernelInfo
parseKernel sourceCode =
  case Parse.run Kernel.parser sourceCode of
    Right info ->
      Right (KernelInfo info)

    Left err ->
      Left (Error (A.map Error.Syntax err))
