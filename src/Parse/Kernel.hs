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

import qualified AST.Expression.Source as Src
import qualified AST.Kernel as Kernel
import qualified AST.Module.Name as Module
import qualified Elm.Name as N
import Generate.JavaScript.Helpers as Help (toFieldName)
import Generate.JavaScript.Variable as Var (intToAscii)
import Parse.Primitives (Parser)
import qualified Parse.Primitives.Kernel as K
import qualified Parse.Primitives.Symbol as Symbol
import qualified Parse.Module as Module
import qualified Reporting.Annotation as A



-- PARSER


parser :: Parser Kernel.Content
parser =
  do  Symbol.jsMultiCommentOpen
      Module.freshLine
      srcImports <- Module.chompImports []
      Symbol.jsMultiCommentClose
      let imports = Map.unions (map destructImport srcImports)
      chunks <- parserHelp imports Map.empty Map.empty []
      return (Kernel.Content (Map.elems imports) chunks)


parserHelp :: Imports -> Enums -> Fields -> [Kernel.Chunk] -> Parser [Kernel.Chunk]
parserHelp imports enums fields chunks =
  do  (javascript, maybeTag) <- K.chunk
      case maybeTag of
        Nothing ->
          return (Kernel.JS javascript : chunks)

        Just tag ->
          case tag of
            K.Prod ->
              parserHelp imports enums fields (Kernel.Prod : Kernel.JS javascript : chunks)

            K.Debug ->
              parserHelp imports enums fields (Kernel.Debug : Kernel.JS javascript : chunks)

            K.Import var ->
              case Map.lookup var imports of
                Nothing ->
                  error ("Bad kernel symbol: " ++ Text.unpack var)

                Just (home, name) ->
                  parserHelp imports enums fields (Kernel.Var home name : Kernel.JS javascript : chunks)

            K.Enum n var ->
              let
                (bytes, newEnums) =
                  lookupEnum n var enums
              in
                parserHelp imports newEnums fields (Kernel.JS bytes : Kernel.JS javascript : chunks)

            K.ElmField name ->
              parserHelp imports enums fields (Kernel.Field name : Kernel.JS javascript : chunks)

            K.JsField name ->
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


destructImport :: Src.Import -> Imports
destructImport (Src.Import (A.A _ moduleName) maybeAlias exposing) =
  let
    shortName =
      case maybeAlias of
        Just alias ->
          alias

        Nothing ->
          if Module.isKernel moduleName then
            Module.getKernel moduleName
          else if Text.isInfixOf "." moduleName then
            error ("modules with dots in kernel code need an alias: " ++ show moduleName)
          else
            moduleName

    toEntry name =
      ( shortName <> "_" <> name, ( moduleName, name ) )
  in
    Map.fromList $ map toEntry $
      case exposing of
        Src.Open ->
          error "cannot have open imports in kernel code"

        Src.Explicit exposed ->
          map exposedToName exposed


exposedToName :: A.Located Src.Exposed -> N.Name
exposedToName (A.A _ exposed) =
  case exposed of
    Src.Lower name ->
      name

    Src.Upper name Src.Private ->
      name

    Src.Upper _ Src.Public ->
      error "cannot have Maybe(..) syntax in kernel code header"

    Src.Operator _ ->
      error "cannot use binops in kernel code"
