{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Kernel
  ( parser
  )
  where


import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)

import qualified AST.Optimized as Opt
import qualified AST.Source as Src
import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import Parse.Primitives (Parser)
import qualified Parse.Primitives.Kernel as K
import qualified Parse.Primitives.Symbol as Symbol
import qualified Parse.Module as Module
import qualified Reporting.Annotation as A



-- PARSER


parser :: Parser ( [(N.Name, N.Name)], [Opt.KChunk] )
parser =
  do  Symbol.jsMultiCommentOpen
      Module.freshLine
      srcImports <- Module.chompImports []
      Symbol.jsMultiCommentClose
      let imports = Map.unions (map destructImport srcImports)
      chunks <- parserHelp imports Map.empty Map.empty []
      return ( Map.elems imports, chunks )


parserHelp :: Imports -> Enums -> Fields -> [Opt.KChunk] -> Parser [Opt.KChunk]
parserHelp imports enums fields chunks =
  do  (javascript, maybeTag) <- K.chunk
      case maybeTag of
        Nothing ->
          return (Opt.JS javascript : chunks)

        Just tag ->
          case tag of
            K.Prod ->
              parserHelp imports enums fields $
                Opt.Prod : Opt.JS javascript : chunks

            K.Debug ->
              parserHelp imports enums fields $
                Opt.Debug : Opt.JS javascript : chunks

            K.Import var ->
              case Map.lookup var imports of
                Nothing ->
                  error ("Bad kernel symbol: " ++ Text.unpack var)

                Just (home, name) ->
                  parserHelp imports enums fields $
                    Opt.Var home name : Opt.JS javascript : chunks

            K.Enum n var ->
              let (enum, newEnums) = lookupEnum n var enums in
              parserHelp imports newEnums fields $
                Opt.Enum enum : Opt.JS javascript : chunks

            K.ElmField name ->
              parserHelp imports enums fields $
                Opt.ElmField name : Opt.JS javascript : chunks

            K.JsField name ->
              let (field, newFields) = lookupField name fields in
              parserHelp imports enums newFields $
                Opt.JsField field : Opt.JS javascript : chunks



-- FIELDS


type Fields =
  Map.Map Text Int


lookupField :: Text -> Fields -> (Int, Fields)
lookupField name fields =
  case Map.lookup name fields of
    Just n ->
      ( n, fields )

    Nothing ->
      let n = Map.size fields in
      ( n, Map.insert name n fields )



-- ENUMS


type Enums =
  Map.Map Word8 (Map.Map N.Name Int)


lookupEnum :: Word8 -> N.Name -> Enums -> (Int, Enums)
lookupEnum word var allEnums =
  let
    enums =
      Map.findWithDefault Map.empty word allEnums
  in
    case Map.lookup var enums of
      Just n ->
        ( n, allEnums )

      Nothing ->
        let n = Map.size enums in
        ( n, Map.insert word (Map.insert var n enums) allEnums )



-- IMPORTS


type Imports =
  Map.Map Text (N.Name, N.Name)


destructImport :: Src.Import -> Imports
destructImport (Src.Import (A.At _ moduleName) maybeAlias exposing) =
  let
    shortName =
      case maybeAlias of
        Just alias ->
          alias

        Nothing ->
          if ModuleName.isKernel moduleName then
            ModuleName.getKernel moduleName
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
exposedToName (A.At _ exposed) =
  case exposed of
    Src.Lower name ->
      name

    Src.Upper name Src.Private ->
      name

    Src.Upper _ Src.Public ->
      error "cannot have Maybe(..) syntax in kernel code header"

    Src.Operator _ ->
      error "cannot use binops in kernel code"
