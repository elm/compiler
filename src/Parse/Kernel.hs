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
import Parse.Helpers (Parser)
import qualified Parse.Helpers as Parse
import qualified Parse.Module as Parse
import qualified Reporting.Annotation as A



-- PARSER


parser :: Parser Kernel.Info
parser =
  do  imports <- Parse.kernelHeader
      let table = Map.unions (map importToTable imports)
      chunks <- parserHelp table Map.empty []
      return (Kernel.Info (Map.elems table) chunks)


type Enums =
  Map.Map Word8 (Map.Map Text BS.ByteString)


parserHelp :: Table -> Enums -> [Kernel.Chunk] -> Parser [Kernel.Chunk]
parserHelp table enums chunks =
  do  (javascript, maybeMore) <- Parse.kernelChunk
      case maybeMore of
        Nothing ->
          return (Kernel.JS javascript : chunks)

        Just (Parse.KernelImport, var) ->
          case Map.lookup var table of
            Nothing ->
              error $ show $ "could not find " <> var <> " when parsing kernel code"

            Just (home, name) ->
              parserHelp table enums (Kernel.Var home name : Kernel.JS javascript : chunks)

        Just (Parse.KernelEnum n, var) ->
          let
            (bytes, newEnums) =
              lookupEnum n var enums
          in
            parserHelp table newEnums (Kernel.JS bytes : Kernel.JS javascript : chunks)


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



-- SYMBOL TABLE


type Table =
  Map.Map Text (Module.Raw, Text)


importToTable :: Module.UserImport -> Table
importToTable (A.A _ ( fullName, Module.ImportMethod maybeAlias exposed )) =
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
      ( "__" <> shortName <> "_" <> value, ( fullName, value ) )
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
