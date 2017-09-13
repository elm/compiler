{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Kernel
  ( parser
  )
  where


import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)

import qualified AST.Exposing as Exposing
import qualified AST.Kernel as Kernel
import qualified AST.Module as Module
import qualified AST.Module.Name as Module
import Generate.JavaScript.Helpers as Help (toFieldName)
import Generate.JavaScript.Variable as Var (intToAscii)
import Parse.Helpers (Parser)
import qualified Parse.Helpers as Parse
import qualified Parse.Module as Parse
import qualified Reporting.Annotation as A



-- PARSER


parser :: Parser Kernel.Info
parser =
  do  header <- Parse.kernelHeader
      let imports = Map.unions (map headerToImport header)
      chunks <- parserHelp imports Map.empty Map.empty []
      return (Kernel.Info (Map.elems imports) chunks)


parserHelp :: Imports -> Enums -> Fields -> [Kernel.Chunk] -> Parser [Kernel.Chunk]
parserHelp imports enums fields chunks =
  do  (javascript, maybeTag) <- Parse.kernelChunk
      case maybeTag of
        Nothing ->
          return (Kernel.JS javascript : chunks)

        Just tag ->
          case tag of
            Parse.KernelProd ->
              parserHelp imports enums fields (Kernel.Prod : Kernel.JS javascript : chunks)

            Parse.KernelDebug ->
              parserHelp imports enums fields (Kernel.Debug : Kernel.JS javascript : chunks)

            Parse.KernelImport var ->
              case Map.lookup var imports of
                Nothing ->
                  error ("Bad kernel symbol: " ++ Text.unpack var)

                Just (home, name) ->
                  parserHelp imports enums fields (Kernel.Var home name : Kernel.JS javascript : chunks)

            Parse.KernelEnum n var ->
              let
                (bytes, newEnums) =
                  lookupEnum n var enums
              in
                parserHelp imports newEnums fields (Kernel.JS bytes : Kernel.JS javascript : chunks)

            Parse.KernelElmField name ->
              parserHelp imports enums fields (Kernel.Field name : Kernel.JS javascript : chunks)

            Parse.KernelJsField name ->
              let
                (bytes, newFields) =
                  lookupField name fields
              in
                parserHelp imports enums newFields (Kernel.JS bytes : Kernel.JS javascript : chunks)



-- FIELDS


type Fields =
  Map.Map Text BS.ByteString


lookupField :: Text -> Fields -> (BS.ByteString, Fields)
lookupField longName fields =
  case Map.lookup longName fields of
    Just bytes ->
      ( bytes, fields )

    Nothing ->
      let
        shortName =
          Var.intToAscii (Map.size fields)
      in
        ( shortName, Map.insert longName shortName fields )



-- ENUMS


type Enums =
  Map.Map Word8 (Map.Map Text BS.ByteString)


lookupEnum :: Word8 -> Text -> Enums -> (BS.ByteString, Enums)
lookupEnum word var allEnums =
  let
    enums =
      Map.findWithDefault Map.empty word allEnums
  in
    case Map.lookup var enums of
      Just bytes ->
        ( bytes, allEnums )

      Nothing ->
        let
          identifier =
            Text.encodeUtf8 ("'" <> Help.toFieldName (Map.size enums) <> "'")
        in
          ( identifier, Map.insert word (Map.insert var identifier enums) allEnums )



-- IMPORTS


type Imports =
  Map.Map Text (Module.Raw, Text)


headerToImport :: Module.UserImport -> Imports
headerToImport (A.A _ ( A.A _ fullName, Module.ImportMethod maybeAlias exposed )) =
  let
    shortName =
      case maybeAlias of
        Just alias ->
          alias

        Nothing ->
          if Module.isKernel fullName then
            Module.getKernel fullName
          else if Text.isInfixOf "." fullName then
            error ("modules with dots in kernel code need an alias: " ++ show fullName)
          else
            fullName

    toEntry value =
      ( shortName <> "_" <> value, ( fullName, value ) )
  in
    Map.fromList $ map toEntry $
      concatMap entryToValues $ getExplicits exposed


getExplicits :: Exposing.Exposing a -> [a]
getExplicits exposed =
  case exposed of
    Exposing.Open ->
      error "cannot have open imports in kernel code"

    Exposing.Explicit entries ->
      map A.drop entries


entryToValues :: Exposing.Entry -> [Text]
entryToValues entry =
  case entry of
    Exposing.Lower name ->
      [name]

    Exposing.Upper name ctors ->
      maybe [name] getExplicits ctors
